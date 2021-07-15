# libraries
library(tidyverse); library(mirt); library(haven); library(mice)
library(gridExtra); library(ggrepel); library(survey); library(knitr)
library(lme4); library(googledrive)

get_gdrive = function(did, fileext) {
  temp = tempfile(fileext = fileext)
  dl = drive_download(as_id(did), path = temp, overwrite = TRUE)
  if(fileext == ".csv") {
    read_csv(dl$local_path)
  } else if(fileext == ".dta") {
    read_dta(dl$local_path)
  } else if(fileext == ".sav") {
    read_sav(dl$local_path)
  }
}

vendorDensity = get_gdrive("1SOxqdkWrLMazDLEaddwLp1sfSDBXpn_D", ".dta")
wave1_kolkata = get_gdrive("1--vqC_OS2zr8-ZtWdzESqZXMzB9n0wRY", ".csv")
wave1_mumbai  = get_gdrive("1udgQLcAsWYBTv2kEB7niIEKhTlKkbV6Y", ".csv")
wave2         = get_gdrive("1-oU7ke7GJnDSxul-uo3Wths5RHrbHX1n", ".sav")
vendorType    = get_gdrive("1gQGYWCEcZYmm02sQGge1ekyw9JPIbKjs", ".csv")

# convert to correct column types
rescale = function(x) {
  value_labels = attr(x, "labels")
  if(!is.null(value_labels)) {
    x = as_factor(x)
  } else {
    x = as.numeric(x)
  }
  return(x)
}

wave2 = wave2 %>% as.data.frame()
# Some wave 1 data required for covariates
# keep only columns of interest
wave1_kolkata = 
  wave1_kolkata %>% 
  select(IV_Unit, iv_unit_id, UFSBlockNo, block_id, household_id, w1a_g1_eng, w1p_a4_eng,
         w1p_a1_eng, w1a_a2_1_eng, w1a_b1_eng, Number_of_HH_nocommerce, IVstrata2, final_weight, 
         blocstrata) %>%
  mutate(city = "Kolkata")

wave1_mumbai = 
  wave1_mumbai %>% 
  select(IV_Unit, iv_unit_id, UFSBlockNo, block_id, household_id, w1a_g1_eng, w1p_a4_eng,
         w1p_a1_eng, w1a_a2_1_eng, w1a_b1_eng, Number_of_HH_nocommerce, IVstrata2, final_weight, 
         blocstrata) %>%
  mutate(city = "Mumbai")

wave1_kolkata %>% 
  left_join(vendorDensity, by = c('IV_Unit' = 'IV_Unit_ID')) ->
  wave1_kolkata
wave1_mumbai %>% 
  left_join(vendorDensity, by = c('IV_Unit' = 'IV_Unit_ID')) ->
  wave1_mumbai

wave1 = bind_rows(wave1_kolkata, wave1_mumbai)
wave2 = wave2 %>% 
  left_join(wave1, by = c('w2_state_id' = 'city',
                          'w2_iv_unit_id' = 'iv_unit_id',
                          'w2_block_id' = 'block_id',
                          'w2_household_id' = 'household_id'))
mumb = wave2 %>% filter(w2_state_id == 'Mumbai')
kol = wave2 %>% filter(w2_state_id == 'Kolkata')

# reverse code a vector coded 1,...,n
rev_code = function(x) {
  x = as.character(x)
  levs = sort(unique(x[complete.cases(x)]))
  rev_levels = levs[length(levs):1]
  levs = as.character(levs)
  rev_levels = as.character(rev_levels)
  names(rev_levels) <- levs
  as.numeric(rev_levels[x])
}

# restrict the valid levels of assessment
restrict = function(x, max) {
  ifelse(x > max, NA, x)
}

# tertiles of a vector
ttile = function(x) {
  cut(x, breaks = quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE, type = 6),
      include.lowest = TRUE) %>% as.numeric()
}

# format data for analysis
kdatcov = 
  kol %>% 
  mutate(IVstrata2 = IVstrata2.x, 
         UFSBlockNo = UFSBlockNo.x,
         IV_Unit = IV_Unit.x,
         blocstrata = blocstrata.x) %>%
  select(w1a_g1_eng, w2a_a1, w2p_a1, w1p_a1_eng, w1a_a2_1_eng,
         Number_of_HH_nocommerce, NumVendors, w2a_a4, IVstrata2, w2p_a5, w2p_a9,
         w2p_c4, w2p_c5, w2p_c6, w2a_b34, w2a_b36, final_weight, IV_Unit, UFSBlockNo, 
         IVstrata2, blocstrata, w2a_b33a, w2a_b33c, w2a_b33d, w2a_b33e, w2_study_weight,
         w2p_c19, w2p_c20, w2a_b26, w2p_a1) %>%
  mutate(w1a_g1_eng_na = ifelse(w1a_g1_eng >= 88, NA, w1a_g1_eng),
         # religionMuslim = ifelse(w1a_g1_eng_na == 1 | w1a_g1_eng_na == 3 | 
         #                           w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 | 
         #                           w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7, 0, 
         #                         w1a_g1_eng_na) %>% as.factor(),
         # religionHindu = ifelse(w1a_g1_eng_na == 2, 0, 
         #                         w1a_g1_eng_na) %>% as.factor(),
         religionMuslim = ifelse(w1a_g1_eng_na == 1 | w1a_g1_eng_na == 3 | 
                                   w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 | 
                                   w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7, 0, 
                                 w1a_g1_eng_na) %>% as.factor(),
         religionHindu = ifelse(w1a_g1_eng_na == 2 | w1a_g1_eng_na == 3 | 
                                  w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 | 
                                  w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7, 0, 
                                w1a_g1_eng_na) %>% as.factor(),
         religionOther = ifelse(w1a_g1_eng_na == 1 | w1a_g1_eng_na == 2, 0, 
                                w1a_g1_eng_na),
         religionOther = ifelse(religionOther == 3 | 
                                  religionOther == 4 | religionOther == 5 | 
                                  religionOther == 6 | religionOther == 7, 1, 
                                religionOther) %>% as.factor(),
         genderChild = ifelse(w2a_a1 == 2, 'female', 'male') %>% as.factor(),
         genderPareFemale = ifelse(w2p_a1 == 2, 'female', 'male') %>% as.factor(),
         # adolLang = a_lang.x %>% as.factor(),
         # parLang = p_lang.x %>% as.factor(),
         num_hh = Number_of_HH_nocommerce / sd(Number_of_HH_nocommerce, na.rm = TRUE),
         childage = ifelse(w1a_a2_1_eng == 15, 14, w1a_a2_1_eng) %>% as.factor(),
         vendor = NumVendors / sd(NumVendors, na.rm = TRUE),
         tvendor = ttile(vendor) %>% as.factor(),
         neighborSES = IVstrata2 %>% as.factor(),
         w2a_a4 = restrict(w2a_a4, max = 8),
         grades = case_when(w2a_a4 == 1 ~ 95,
                            w2a_a4 == 2 ~ 85,
                            w2a_a4 == 3 ~ 75,
                            w2a_a4 == 4 ~ 65,
                            w2a_a4 == 5 ~ 55,
                            w2a_a4 == 6 ~ 45,
                            w2a_a4 == 7 ~ 35,
                            w2a_a4 == 8 ~ 25),
         w2p_c4 = ifelse(is.na(w2p_c4), 0, w2p_c4),
         w2p_c5 = ifelse(is.na(w2p_c5), 0, w2p_c5),
         w2p_c6 = ifelse(is.na(w2p_c6), 0, w2p_c6),
         w2p_c4 = (restrict(w2p_c4, max = 2) > 1) * 1,
         w2p_c5 = (restrict(w2p_c5, max = 2) > 1) * 1,
         w2p_c6 = (restrict(w2p_c6, max = 2) > 1) * 1,
         w2p_c19 = (restrict(w2p_c19, max = 2) == 1) * 1,
         w2p_c20 = (restrict(w2p_c20, max = 2) == 1) * 1,
         parUse = (w2p_c4 + w2p_c5 + w2p_c6 + w2p_c19 + w2p_c20) > 0,
         prim_ed = ifelse(w2p_a5 == 5 | w2p_a5 == 6, 4, w2p_a5),
         spous_ed = restrict(w2p_a9, max = 10),
         spous_ed = ifelse(spous_ed == 5 | spous_ed == 6, 4, spous_ed),
         max_ed = pmax(prim_ed, spous_ed),
         max_ed = ifelse(is.na(max_ed), ifelse(!is.na(prim_ed), prim_ed, spous_ed), max_ed),
         max_ed = ifelse(max_ed < 3, 0, max_ed),
         max_ed = ifelse(max_ed == 3, 1, max_ed),
         max_ed = ifelse(max_ed > 3, 2, max_ed) %>% as.factor(),
         comb_ed = ifelse(prim_ed < 3 & spous_ed < 3, "both_low",
                          ifelse(prim_ed >= 3 & spous_ed >= 3, "both_high", "one_or_more")),
         ed_1or_more = 1*(comb_ed == "one_or_more"),
         ed_both_high = 1*(comb_ed == "both_high"),
         perc_vendor_home = restrict(w2a_b34, max = 4),
         perc_vendor_home = case_when(perc_vendor_home == 1 ~ "None",
                                      perc_vendor_home == 2 ~ "A few",
                                      perc_vendor_home == 3 ~ "A lot"),
         perc_vend_home_a_little = (perc_vendor_home == "A few") * 1,
         perc_vend_home_a_lot = (perc_vendor_home == "A lot") * 1,
         perc_vendor_school = restrict(w2a_b36, max = 4),
         perc_vendor_school = case_when(perc_vendor_school == 1 ~ "None",
                                        perc_vendor_school == 2 ~ "A few",
                                        perc_vendor_school == 3 ~ "A lot"),
         perc_vend_sch_a_little = (perc_vendor_school == "A few") * 1,
         perc_vend_sch_a_lot = (perc_vendor_school == "A lot") * 1,
         final_weight_stz = final_weight / sd(final_weight, na.rm = TRUE),
         w2a_b33a = as.factor(w2a_b33a), 
         w2a_b33c = as.factor(w2a_b33c),
         w2a_b33d = as.factor(w2a_b33d),
         w2a_b33e = as.factor(w2a_b33e),
         inside_home = as.factor((w2a_b26 == 1 | w2a_b26 == 2) * 1),
         father_use = ifelse(w2p_a1 == 1, 
                             ((w2p_c4 + w2p_c5 + w2p_c6) > 0) * 1, 
                             ((w2p_c19 + w2p_c20) > 0) * 1))

# format data for analysis
mdatcov = 
  mumb %>% 
  mutate(IVstrata2 = IVstrata2.x, 
         UFSBlockNo = UFSBlockNo.x,
         IV_Unit = IV_Unit.x,
         blocstrata = blocstrata.x) %>%
  select(w1a_g1_eng, w2a_a1, w2p_a1, w1p_a1_eng, w1a_a2_1_eng,
         Number_of_HH_nocommerce, NumVendors, w2a_a4, IVstrata2, w2p_a5, w2p_a9,
         w2p_c4, w2p_c5, w2p_c6, w2a_b34, w2a_b36, final_weight, IV_Unit, UFSBlockNo, 
         IVstrata2, blocstrata, w2a_b33a, w2a_b33c, w2a_b33d, w2a_b33e, w2_study_weight,
         w2p_c19, w2p_c20, w2a_b26, w2p_a1) %>%
  mutate(w1a_g1_eng_na = ifelse(w1a_g1_eng >= 88, NA, w1a_g1_eng),
         religionMuslim = ifelse(w1a_g1_eng_na == 1 | w1a_g1_eng_na == 3 | 
                                   w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 | 
                                   w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7, 0, 
                                 w1a_g1_eng_na) %>% as.factor(),
         religionHindu = ifelse(w1a_g1_eng_na == 2 | w1a_g1_eng_na == 3 | 
                                  w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 | 
                                  w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7, 0, 
                                w1a_g1_eng_na) %>% as.factor(),
         religionOther = ifelse(w1a_g1_eng_na == 1 | w1a_g1_eng_na == 2, 0, 
                                w1a_g1_eng_na),
         religionOther = ifelse(religionOther == 3 | 
                                  religionOther == 4 | religionOther == 5 | 
                                  religionOther == 6 | religionOther == 7, 1, 
                                religionOther) %>% as.factor(),
         genderChild = ifelse(w2a_a1 == 2, 'female', 'male') %>% as.factor(),
         genderPareFemale = ifelse(w2p_a1 == 2, 'female', 'male') %>% as.factor(),
         # adolLang = a_lang.x %>% as.factor(),
         # parLang = p_lang.x %>% as.factor(),
         num_hh = Number_of_HH_nocommerce / sd(Number_of_HH_nocommerce, na.rm = TRUE),
         childage = ifelse(w1a_a2_1_eng == 15, 14, w1a_a2_1_eng) %>% as.factor(),
         vendor = NumVendors / sd(NumVendors, na.rm = TRUE),
         tvendor = ttile(vendor) %>% as.factor(),
         neighborSES = IVstrata2 %>% as.factor(),
         w2a_a4 = restrict(w2a_a4, max = 8),
         grades = case_when(w2a_a4 == 1 ~ 95,
                            w2a_a4 == 2 ~ 85,
                            w2a_a4 == 3 ~ 75,
                            w2a_a4 == 4 ~ 65,
                            w2a_a4 == 5 ~ 55,
                            w2a_a4 == 6 ~ 45,
                            w2a_a4 == 7 ~ 35,
                            w2a_a4 == 8 ~ 25),
         w2p_c4 = ifelse(is.na(w2p_c4), 0, w2p_c4),
         w2p_c5 = ifelse(is.na(w2p_c5), 0, w2p_c5),
         w2p_c6 = ifelse(is.na(w2p_c6), 0, w2p_c6),
         w2p_c4 = (restrict(w2p_c4, max = 2) > 1) * 1,
         w2p_c5 = (restrict(w2p_c5, max = 2) > 1) * 1,
         w2p_c6 = (restrict(w2p_c6, max = 2) > 1) * 1,
         w2p_c19 = (restrict(w2p_c19, max = 2) == 1) * 1,
         w2p_c20 = (restrict(w2p_c20, max = 2) == 1) * 1,
         parUse = (w2p_c4 + w2p_c5 + w2p_c6 + w2p_c19 + w2p_c20) > 0,
         prim_ed = ifelse(w2p_a5 == 5 | w2p_a5 == 6, 4, w2p_a5),
         spous_ed = restrict(w2p_a9, max = 10),
         spous_ed = ifelse(spous_ed == 5 | spous_ed == 6, 4, spous_ed),
         max_ed = pmax(prim_ed, spous_ed),
         max_ed = ifelse(is.na(max_ed), ifelse(!is.na(prim_ed), prim_ed, spous_ed), max_ed),
         max_ed = ifelse(max_ed < 3, 0, max_ed),
         max_ed = ifelse(max_ed == 3, 1, max_ed),
         max_ed = ifelse(max_ed > 3, 2, max_ed) %>% as.factor(),
         comb_ed = ifelse(prim_ed < 3 & spous_ed < 3, "both_low",
                          ifelse(prim_ed >= 3 & spous_ed >= 3, "both_high", "one_or_more")),
         ed_1or_more = 1*(comb_ed == "one_or_more"),
         ed_both_high = 1*(comb_ed == "both_high"),
         perc_vendor_home = restrict(w2a_b34, max = 4),
         perc_vendor_home = case_when(perc_vendor_home == 1 ~ "None",
                                      perc_vendor_home == 2 ~ "A few",
                                      perc_vendor_home == 3 ~ "A lot"),
         perc_vend_home_a_little = (perc_vendor_home == "A few") * 1,
         perc_vend_home_a_lot = (perc_vendor_home == "A lot") * 1,
         perc_vendor_school = restrict(w2a_b36, max = 4),
         perc_vendor_school = case_when(perc_vendor_school == 1 ~ "None",
                                        perc_vendor_school == 2 ~ "A few",
                                        perc_vendor_school == 3 ~ "A lot"),
         perc_vend_sch_a_little = (perc_vendor_school == "A few") * 1,
         perc_vend_sch_a_lot = (perc_vendor_school == "A lot") * 1,
         final_weight_stz = final_weight / sd(final_weight, na.rm = TRUE),
         w2a_b33a = as.factor(w2a_b33a), 
         w2a_b33c = as.factor(w2a_b33c),
         w2a_b33d = as.factor(w2a_b33d),
         w2a_b33e = as.factor(w2a_b33e),
         inside_home = as.factor((w2a_b26 == 1 | w2a_b26 == 2) * 1),
         father_use = ifelse(w2p_a1 == 1, 
            ((w2p_c4 + w2p_c5 + w2p_c6) > 0) * 1, 
            ((w2p_c19 + w2p_c20) > 0) * 1))

# intention, use, access, self-efficacy
kdat = 
  kol %>%
  select(w2a_b17, w2a_b18, w2a_b19, w2a_b30, w2a_b20, w2a_b21) %>%
  mutate(w2a_b17 = restrict(w2a_b17, 4),
         w2a_b18 = restrict(w2a_b18, 3),
         w2a_b19 = restrict(w2a_b19, 4),
         w2a_b30 = restrict(w2a_b30, 4),
         w2a_b20 = restrict(w2a_b20, 4),
         w2a_b21 = restrict(w2a_b21, 4),
         intention = ifelse(w2a_b17 + w2a_b18 <= 2, 0, 1) %>% as.factor(),
         perc_access = (1*(w2a_b19 == 1)) %>% as.factor(),
         perc_norms = ifelse(w2a_b30 > 1, 1, 0) %>% as.factor(),
         refusal = rev_code(ifelse(w2a_b20 + w2a_b21 <= 2, 0, 1)) %>% as.factor()) %>%
  select(intention, perc_access, perc_norms, refusal)

mdat = 
  mumb %>%
  select(w2a_b17, w2a_b18, w2a_b19, w2a_b30, w2a_b20, w2a_b21) %>%
  mutate(w2a_b17 = restrict(w2a_b17, 4),
         w2a_b18 = restrict(w2a_b18, 3),
         w2a_b19 = restrict(w2a_b19, 4),
         w2a_b30 = restrict(w2a_b30, 4),
         w2a_b20 = restrict(w2a_b20, 4),
         w2a_b21 = restrict(w2a_b21, 4),
         intention = ifelse(w2a_b17 + w2a_b18 <= 2, 0, 1) %>% as.factor(),
         perc_access = (1*(w2a_b19 == 1)) %>% as.factor(),
         perc_norms = ifelse(w2a_b30 > 1, 1, 0) %>% as.factor(),
         refusal = rev_code(ifelse(w2a_b20 + w2a_b21 <= 2, 0, 1)) %>% as.factor()) %>%
  select(intention, perc_access, perc_norms, refusal)

# combine survey and predictor data
mdata_all = bind_cols(mdat, mdatcov)
kdata_all = bind_cols(kdat, kdatcov)

# append city id
mdata_all = mdata_all %>% mutate(city = 'Mumbai')
kdata_all = kdata_all %>% mutate(city = 'Kolkata')

# Frequency of store visits index
set.seed(7794)
kdatstore = 
  kdata_all %>% 
  select(w2a_b33a, w2a_b33c, w2a_b33d, w2a_b33e) %>%
  mutate_all(function(x) {ifelse(is.na(x), 0, x)})

mdatstore = 
  mdata_all %>%
  select(w2a_b33a, w2a_b33c, w2a_b33d, w2a_b33e) %>%
  mutate_all(function(x) {ifelse(is.na(x), 0, x)})

kresults.pcm <- mirt(data = kdatstore, model = 1, itemtype = "gpcm", 
                     SE = TRUE, verbose = FALSE)
mresults.pcm <- mirt(data = mdatstore, model = 1, itemtype = "gpcm", 
                     SE = TRUE, verbose = FALSE)

M2(kresults.pcm, type = "C2")
M2(mresults.pcm, type = "C2")

kol_fs = fscores(kresults.pcm, method = "MAP") %>% as.vector()
mumb_fs = fscores(mresults.pcm, method = "MAP") %>% as.vector()

kdata_all = 
  kdata_all %>%
  bind_cols(store_visit_index = kol_fs)
mdata_all = 
  mdata_all %>%
  bind_cols(store_visit_index = mumb_fs)

# combine cities to test for city differences
Baseline_child = 
  bind_rows(mdata_all, kdata_all)
  # rename(IV_Unit = IV_Unit.x, UFSBlockNo = UFSBlockNo.x,
  #        IVstrata2 = IVstrata2.x, blocstrata = blocstrata.x)

# full survey design object
data_wave2_missing = TRUE
if(data_wave2_missing == TRUE) {
  syv_full = svydesign(ids = ~ IV_Unit + UFSBlockNo, 
                       strata = ~ IVstrata2 + blocstrata,
                       weights = ~ w2_study_weight,
                       data = Baseline_child)
}

# count summaries by group
gn = function(data, column_name) {
  quo_var <- enquo(column_name)
  data %>% group_by(!!column_name) %>% summarise(n())
}

# Test differences for each variable
test_diff = function(col, data_all, syv_full_arg = syv_full) {
  
  mumb_col = data_all %>% filter(city == "Mumbai") %>% pull({{col}})
  kol_col = data_all %>% filter(city == "Kolkata") %>% pull({{col}})
  
  cat("Mumbai raw proportions \n")
  print(table(mumb_col)); print(table(mumb_col) / sum(table(mumb_col)) * 100)
  cat("Kolkata raw proportions \n")
  print(table(kol_col)); print(table(kol_col) / sum(table(kol_col)) * 100)
  
  cat("Test between cities \n")
  print(paste(c("~city", col), collapse = "+"))
  # svychisq(as.formula(paste(c("~city", col), collapse = "+")), design = syv_full_arg, 
  #                statistic = c("F"), na.rm = TRUE) %>% print()
  
  cat("Test by city \n")
  cat("Mumbai \n")
  syvtemp_Mumbai = svydesign(ids = ~ IV_Unit + UFSBlockNo, 
                             strata = ~ IVstrata2 + blocstrata,
                             weights = ~ final_weight_stz,
                             data = data_all[!is.na(data_all$IV_Unit),] %>% filter(city == 'Mumbai'))
  svymean(as.formula(paste(c("~", col))), syvtemp_Mumbai, na.rm = TRUE) %>% print()
  #print(gn(data_all %>% filter(city == 'Mumbai'), column_name = as.symbol(col)))
  
  cat("Kolkata \n")
  syvtemp_Kolkata = svydesign(ids = ~ IV_Unit + UFSBlockNo, 
                              strata = ~ IVstrata2 + blocstrata,
                              weights = ~ final_weight_stz,
                              data = data_all[!is.na(data_all$IV_Unit),] %>% filter(city == 'Kolkata'))
  svymean(as.formula(paste(c("~", col))), syvtemp_Kolkata, na.rm = TRUE) %>% print()
  #print(gn(data_all %>% filter(city == 'Kolkata'), column_name = as.symbol(col)))
  
  # proportion of missing values
  cat("Mumbai \n")
  cat("\n percent missing values \n")
  print(data_all %>% filter(city == 'Mumbai') %>% pull({{col}}) %>% table(useNA = "always") %>% prop.table() * 100)
  cat("\n number of missing \n")
  print(data_all %>% filter(city == 'Mumbai') %>% pull({{col}}) %>% table(useNA = "always"))
  
  cat("Kolkata \n")
  cat("\n percent missing values \n")
  print(data_all %>% filter(city == 'Kolkata') %>% pull({{col}}) %>% table(useNA = "always") %>% prop.table() * 100)
  cat("\n number of missing \n")
  print(data_all %>% filter(city == 'Kolkata') %>% pull({{col}}) %>% table(useNA = "always"))
  
}

numeric_summary = function(data, col) {
  cat("Mumbai \n")
  cat('mean \n')
  print(mean(data %>% filter(city == "Mumbai") %>% pull({{col}}), na.rm = TRUE))
  cat('sd \n')
  print(sd(data %>% filter(city == "Mumbai") %>% pull({{col}}), na.rm = TRUE))
  cat('range \n')
  print(range(data %>% filter(city == "Mumbai") %>% pull({{col}}), na.rm = TRUE))
  cat("Kolkata \n")
  cat('mean \n')
  print(mean(data %>% filter(city == "Kolkata") %>% pull({{col}}), na.rm = TRUE)) 
  cat('sd \n')
  print(sd(data %>% filter(city == "Kolkata") %>% pull({{col}}), na.rm = TRUE))
  cat('range \n')
  print(range(data %>% filter(city == "Kolkata") %>% pull({{col}}), na.rm = TRUE))
}


# test differences for each variable
test_diff(col = "genderChild", data_all = Baseline_child)
test_diff(col = "childage", data_all = Baseline_child)
test_diff(col = "religionMuslim", data_all = Baseline_child)
test_diff(col = "comb_ed", data_all = Baseline_child)
test_diff(col = "perc_vendor_home", data_all = Baseline_child)
test_diff(col = "perc_vendor_school", data_all = Baseline_child)
test_diff(col = "perc_access", data_all = Baseline_child)
test_diff(col = "perc_norms", data_all = Baseline_child)
test_diff(col = "intention", data_all = Baseline_child)

# certain one-off stats presented in the text of the paper
test_diff(col = "religionHindu", data_all = Baseline_child)
test_diff(col = "religionOther", data_all = Baseline_child)
test_diff(col = "father_use", data = Baseline_child)
test_diff(col = "inside_home", data = Baseline_child)

# test_diff(col = "parUse", data_all = Baseline_child)
# test_diff(col = "grades", data_all = Baseline_child)
# numeric_summary(Baseline_child, "grades")
# test_diff(col = "vendor", data_all = Baseline_child)
# test_diff(col = "tvendor", data_all = Baseline_child)
# test_diff(col = "refusal", data_all = Baseline_child)
# test_diff(col = "NumVendors", data_all = Baseline_child)
# test_diff(col = "num_hh", data_all = Baseline_child)
test_diff(col = "w2a_b33a", data_all = Baseline_child)
test_diff(col = "w2a_b33c", data_all = Baseline_child)
test_diff(col = "w2a_b33d", data_all = Baseline_child)
test_diff(col = "w2a_b33e", data_all = Baseline_child)

Baseline_child2 = 
  Baseline_child %>% 
  mutate(w2a_b33a = as.numeric(paste(w2a_b33a)),
         w2a_b33c = as.numeric(paste(w2a_b33c)),
         w2a_b33d = as.numeric(paste(w2a_b33d)),
         w2a_b33e = as.numeric(paste(w2a_b33e)),
         visit_store = ifelse(w2a_b33a > 2, 1, 0),
         visit_store = ifelse(w2a_b33c > 2, 1, visit_store),
         visit_store = ifelse(w2a_b33d > 2, 1, visit_store),
         visit_store = ifelse(w2a_b33e > 2, 1, visit_store),
         visit_store = as.factor(visit_store))
test_diff(col = "visit_store", data_all = Baseline_child2)


Baseline_child %>% 
  group_by(city) %>% 
  select(store_visit_index) %>% 
  summarise(m = mean(store_visit_index), 
            sd = sd(store_visit_index))
test_diff(col = "store_visit_index", data_all = Baseline_child)


#vendorType = vendorType %>% mutate(uniqueID = 1:nrow(vendorType))

# Baseline_child_type = left_join(Baseline_child, vendorType,
#                                 by = c("IV_Unit" = "IV_Unit_ID"))

# total number of mapped tobacco retailers
vendorType %>% 
  group_by(CityName) %>% 
  filter(!(IV_Unit_ID == 9778)) %>% 
  summarise(nretailer = n())

neighborhood_mean_type = 
  vendorType %>% 
  group_by(CityName, IV_Unit_ID, Vendor_Type) %>%
  filter(!(IV_Unit_ID == 9778)) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(CityName, Vendor_Type) %>%
  summarise(count = sum(n),
            ave_vend = mean(n),
            sd = sd(n),
            range = range(n)[1],
            range2 = range(n)[2])

chi_data_kol = neighborhood_mean_type %>% filter(CityName == "Kolkata") %>%
  select(CityName, Vendor_Type, count) %>% drop_na() %>% arrange(Vendor_Type)

chi_data_mumb = neighborhood_mean_type %>% filter(CityName == "Mumbai") %>%
  select(CityName, Vendor_Type, count) %>%
  bind_rows(data.frame(CityName = c("Mumbai", "Mumbai"), Vendor_Type = c(4, 8), 
                       count = c(0, 0))) %>% drop_na() %>% arrange(Vendor_Type)

chi_data = cbind(chi_data_kol %>% pull(count),
                 chi_data_mumb %>% pull(count))

neighborhood_mean_type
chi_data
chisq.test(chi_data)

neighborhood_mean_overall = 
  vendorType %>% 
  group_by(CityName, IV_Unit_ID) %>%
  filter(!(IV_Unit_ID == 9778)) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(CityName) %>%
  summarise(ave_vend = mean(n),
            sd = sd(n),
            range = range(n)[1],
            range2 = range(n)[2])

nh_mean_overall = 
  vendorType %>% 
  group_by(CityName, IV_Unit_ID) %>%
  filter(!(IV_Unit_ID == 9778)) %>%
  summarise(n = n())

t.test(
  x = nh_mean_overall %>% 
    filter(CityName == "Mumbai") %>% 
    pull(n),
  y = nh_mean_overall %>% 
    filter(CityName == "Kolkata") %>% 
    pull(n)
)

chi_data = data.frame(
  x1 = neighborhood_mean_overall %>% 
    filter(CityName == "Kolkata") %>% 
    pull(ave_vend),
  x2 = neighborhood_mean_overall %>% 
    filter(CityName == "Mumbai") %>% 
    pull(ave_vend)
  )

neighborhood_mean_overall
chi_data
chisq.test(chi_data)

vendorType %>% 
  filter(IV_Unit_ID != 9778) %>% 
  group_by(CityName) %>%
  summarise(n = n())

kdatcov %>%
  group_by(neighborSES, UFSBlockNo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(neighborSES) %>%
  summarise(n = n())

mdatcov %>%
  group_by(neighborSES, UFSBlockNo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(neighborSES) %>%
  summarise(n = n())


# Perceived tobacco retail density near home: mean (range)
neighborhood_mean_perc_home = Baseline_child %>%
  group_by(city, IV_Unit, perc_vendor_home) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(city, perc_vendor_home) %>%
  summarise(count = sum(n),
            ave = mean(n),
            sd = sd(n),
            range = range(n)[1],
            range2 = range(n)[2])

chi_data = data.frame(x1 = neighborhood_mean_perc_home %>% filter(city == "Kolkata") 
                      %>% pull(count),
                      x2 = neighborhood_mean_perc_home %>% filter(city == "Mumbai") 
                      %>% pull(count))

neighborhood_mean_perc_home
chi_data
chisq.test(chi_data[-4,])

# Perceived tobacco retail density near school: mean (range)
neighborhood_mean_perc_school = Baseline_child %>%
  group_by(city, IV_Unit, perc_vendor_school) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(city, perc_vendor_school) %>%
  summarise(count = sum(n),
            ave = mean(n),
            sd = sd(n),
            range = range(n)[1],
            range2 = range(n)[2])

chi_data = data.frame(x1 = neighborhood_mean_perc_school %>% filter(city == "Kolkata") 
                      %>% pull(count),
                      x2 = neighborhood_mean_perc_school %>% filter(city == "Mumbai") 
                      %>% pull(count))

neighborhood_mean_perc_school
chi_data
chisq.test(chi_data[-4,])

# Investigator unit socioeconomic status: mean (range)
neighborhood_strata = Baseline_child %>%
  group_by(city, IV_Unit, IVstrata2) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(city, IVstrata2) %>%
  summarise(count = sum(n),
            ave = mean(n),
            sd = sd(n),
            range = range(n)[1],
            range2 = range(n)[2])

chi_data = data.frame(x1 = neighborhood_strata %>% filter(city == "Kolkata") 
                      %>% pull(count),
                      x2 = neighborhood_strata %>% filter(city == "Mumbai") 
                      %>% pull(count))

neighborhood_strata
chi_data
chisq.test(chi_data[-5,])

# what percentage of the tables contain missing data overall?
sum(!complete.cases(Baseline_child %>% filter(city == "Mumbai") %>% 
  select(genderChild, childage, religionMuslim, 
  perc_vendor_home, perc_vendor_school, perc_norms, perc_access, intention, w2a_b33a,
  w2a_b33c, w2a_b33d, w2a_b33e)))
sum(!complete.cases(Baseline_child %>% filter(city == "Kolkata") %>% 
                      select(genderChild, childage, religionMuslim, 
                             perc_vendor_home, perc_vendor_school, perc_norms, perc_access, intention, w2a_b33a,
                             w2a_b33c, w2a_b33d, w2a_b33e)))

