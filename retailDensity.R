# libraries
library(tidyverse); library(mirt); library(haven); library(mice)
library(gridExtra); library(ggrepel); library(survey); library(knitr)
library(lme4); library(naniar); library(googledrive); library(polycor)

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
# mutate(w2_state_id = rescale(w2_state_id))
wave2 = wave2 %>% as.data.frame()
# Some wave 1 data required for covariates
# keep only columns of interest
wave1_kolkata = 
  wave1_kolkata %>% 
  select(IV_Unit, iv_unit_id, UFSBlockNo, block_id, household_id, w1a_g1_eng, w1p_a4_eng, w1a_b1_eng,
         w1p_a1_eng, w1a_a2_1_eng, Number_of_HH_nocommerce, IVstrata2, final_weight, 
         blocstrata) %>%
  mutate(city = "Kolkata")

wave1_mumbai = 
  wave1_mumbai %>% 
  select(IV_Unit, iv_unit_id, UFSBlockNo, block_id, household_id, w1a_g1_eng, w1p_a4_eng, w1a_b1_eng,
         w1p_a1_eng, w1a_a2_1_eng, Number_of_HH_nocommerce, IVstrata2, final_weight, 
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

# wealth index
set.seed(7794)
kdatw = 
  kol %>% 
  select(w2hh_a3a, w2hh_a3b, w2hh_a3c, 
         w2hh_a3d, w2hh_a3e, w2hh_a3f,
         w2hh_a3g, w2hh_a3h, w2hh_a3i) %>%
  mutate_all(function(x) {ifelse(x > 2, NA, x)}) %>%
  mutate_all(function(x) {ifelse(is.na(x), 0, x)})

mdatw = 
  mumb %>%
  select(w2hh_a3a, w2hh_a3b, w2hh_a3c, 
         w2hh_a3d, w2hh_a3e, w2hh_a3f,
         w2hh_a3g, w2hh_a3h, w2hh_a3i) %>%
  mutate_all(function(x) {ifelse(x > 2, NA, x)}) %>%
  mutate_all(function(x) {ifelse(is.na(x), 0, x)})

kresults.pcm <- mirt(data = kdatw, model = 1, itemtype = "gpcm", 
                     SE = TRUE, verbose = FALSE)
mresults.pcm <- mirt(data = mdatw, model = 1, itemtype = "gpcm", 
                     SE = TRUE, verbose = FALSE)

kol_fs = fscores(kresults.pcm, method = "MAP") %>% as.vector()
mumb_fs = fscores(mresults.pcm, method = "MAP") %>% as.vector()

kol = 
  kol %>%
  bind_cols(wealth_index = kol_fs)
mumb = 
  mumb %>%
  bind_cols(wealth_index = mumb_fs)

rm(kol_fs, mumb_fs)

# Frequency of store visits index
set.seed(7794)
kdatw = 
  kol %>% 
  select(w2a_b33a, w2a_b33c, w2a_b33d, w2a_b33e) %>%
  mutate_all(function(x) {ifelse(is.na(x), 0, x)})

mdatw = 
  mumb %>%
  select(w2a_b33a, w2a_b33c, w2a_b33d, w2a_b33e) %>%
  mutate_all(function(x) {ifelse(is.na(x), 0, x)})

kresults.pcm <- mirt(data = kdatw, model = 1, itemtype = "gpcm", 
                     SE = TRUE, verbose = FALSE)
mresults.pcm <- mirt(data = mdatw, model = 1, itemtype = "gpcm", 
                     SE = TRUE, verbose = FALSE)

kol_fs = fscores(kresults.pcm, method = "MAP") %>% as.vector()
mumb_fs = fscores(mresults.pcm, method = "MAP") %>% as.vector()

kol = 
  kol %>%
  bind_cols(store_visit_index = kol_fs)
mumb = 
  mumb %>%
  bind_cols(store_visit_index = mumb_fs)

# # remove ever users from the study
# kol = 
#   kol %>% 
#   filter(w2a_b1 != 2, w2a_b1 != 3, w1a_b1_eng != 1, w1a_b1_eng != 2)
# mumb = 
#   mumb %>% 
#   filter(w2a_b1 != 2, w2a_b1 != 3, w1a_b1_eng != 1, w1a_b1_eng != 2)

# process predictors of intention, use, access, self-efficacy
kdatcov = 
  kol %>% 
  mutate(IVstrata2 = IVstrata2.x, 
         UFSBlockNo = UFSBlockNo.x,
         IV_Unit = IV_Unit.x,
         blocstrata = blocstrata.x) %>%
  select(w1a_g1_eng, w2a_a1, w2p_a1, w1p_a1_eng, w1a_a2_1_eng, wealth_index, store_visit_index,
         Number_of_HH_nocommerce, NumVendors, w2a_a4, w2p_a5, w2p_a9,
         w2p_c4, w2p_c5, w2p_c6, w2a_b34, w2a_b36, final_weight, IV_Unit, UFSBlockNo, 
         IVstrata2, blocstrata, w2_study_weight, w2p_c19, w2p_c20) %>%
  mutate(w1a_g1_eng_na = ifelse(w1a_g1_eng >= 88, NA, w1a_g1_eng),
         religionMuslim = ifelse(w1a_g1_eng_na == 1 | w1a_g1_eng_na == 3 | 
                                   w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 | 
                                   w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7, 0, 
                                 w1a_g1_eng_na) %>% as.factor(),
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
         IVstrata2 = as.factor(IVstrata2)) %>% 
  group_by(IV_Unit) %>% mutate(num_hh_IV = sum(num_hh)) %>% ungroup()

mdatcov = 
  mumb %>% 
  mutate(IVstrata2 = IVstrata2.x, 
         UFSBlockNo = UFSBlockNo.x,
         IV_Unit = IV_Unit.x,
         blocstrata = blocstrata.x) %>%
  select(w1a_g1_eng, w2a_a1, w2p_a1, w1p_a1_eng, w1a_a2_1_eng, wealth_index, store_visit_index,
         Number_of_HH_nocommerce, NumVendors, w2a_a4, w2p_a5, w2p_a9,
         w2p_c4, w2p_c5, w2p_c6, w2a_b34, w2a_b36, final_weight, IV_Unit, UFSBlockNo, 
         IVstrata2, blocstrata, w2_study_weight, w2p_c19, w2p_c20) %>%
  mutate(w1a_g1_eng_na = ifelse(w1a_g1_eng >= 88, NA, w1a_g1_eng),
         # religionMuslim = ifelse(w1a_g1_eng_na == 1 | w1a_g1_eng_na == 3 |
         #                           w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 |
         #                           w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7, 0,
         #                         w1a_g1_eng_na) %>% as.factor(),
         religionMuslim = case_when(w1a_g1_eng_na == 1 ~ "Hindu",
                                    w1a_g1_eng_na == 2 ~ "Muslim",
                                    w1a_g1_eng_na == 3 |
                                      w1a_g1_eng_na == 4 | w1a_g1_eng_na == 5 |
                                      w1a_g1_eng_na == 6 | w1a_g1_eng_na == 7 ~ "Other") %>%
           as.factor(),
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
         IVstrata2 = as.factor(IVstrata2)) %>% 
  group_by(IV_Unit) %>% mutate(num_hh_IV = sum(num_hh)) %>% ungroup()

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
         intention = ifelse(w2a_b17 + w2a_b18 <= 2, 0, 1),
         perc_access = 1*(w2a_b19 == 1),
         perc_norms = ifelse(w2a_b30 > 1, 1, 0),
         refusal = rev_code(ifelse(w2a_b20 + w2a_b21 <= 2, 0, 1))) %>%
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
         intention = ifelse(w2a_b17 + w2a_b18 <= 2, 0, 1),
         perc_access = 1*(w2a_b19 == 1 | w2a_b19 == 2),
         perc_norms = ifelse(w2a_b30 > 1, 1, 0),
         refusal = rev_code(ifelse(w2a_b20 + w2a_b21 <= 2, 0, 1))) %>%
  select(intention, perc_access, perc_norms, refusal)

# inspect missingness
vis = FALSE
if(vis == TRUE) {
  vis_miss(mdata_all)
  vis_miss(kdata_all)
}

het = FALSE
if(het == TRUE) {
  hkol = hetcor(
    bind_cols(kdat %>% select(-refusal), 
      kdatcov %>% 
        select(perc_vendor_home, perc_vendor_school, store_visit_index, 
               genderChild, childage, religionMuslim, tvendor)
      ) %>% mutate(perc_vendor_home = relevel(as.factor(perc_vendor_home), "None"),
                   perc_vendor_school = relevel(as.factor(perc_vendor_school), "None"),
                   intention = as.factor(intention),
                   perc_access = as.factor(perc_access),
                   perc_norms = as.factor(perc_norms)),
    use = "pairwise.complete.obs"
    )
  hkol_hetcor = hkol$correlations %>% as.data.frame()
  write_csv(hkol_hetcor, "kol_hetcor.csv")
  
  hmumb = hetcor(
    bind_cols(mdat %>% select(-refusal), 
              mdatcov %>% 
                select(perc_vendor_home, perc_vendor_school, store_visit_index, 
                       genderChild, childage, religionMuslim, tvendor)
    ) %>% mutate(perc_vendor_home = relevel(as.factor(perc_vendor_home), "None"),
                 perc_vendor_school = relevel(as.factor(perc_vendor_school), "None"),
                 intention = as.factor(intention),
                 perc_access = as.factor(perc_access),
                 perc_norms = as.factor(perc_norms)),
    use = "pairwise.complete.obs"
  )
  hmumb_hetcor = hmumb$correlations %>% as.data.frame()
  write_csv(hmumb_hetcor, "mumb_hetcor.csv")
}

# complete cases or impute
format_data_mice = function(data) {
  data %>% mutate_all(.funs = function(x) {
    if(!is.null(attr(x, "class"))) {
      if("haven_labelled" %in% attr(x, "class")) {
        attr(x, "class") = attr(x, "class")[length(attr(x, "class"))]
        return(x)
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  })
}

mdata_all = as.data.frame(bind_cols(mdat, mdatcov)) %>%
  select(intention:refusal, wealth_index:NumVendors, IVstrata2, 
         IV_Unit:blocstrata, religionMuslim:childage, tvendor:parUse, ed_1or_more:ed_both_high,
         perc_vend_home_a_little:perc_vend_home_a_lot, 
         perc_vend_sch_a_little:perc_vend_sch_a_lot, w2_study_weight,
         num_hh_IV, store_visit_index) %>%
  format_data_mice

kdata_all = as.data.frame(bind_cols(kdat, kdatcov)) %>%
  select(intention:refusal, wealth_index:NumVendors, IVstrata2, 
         IV_Unit:blocstrata, religionMuslim:childage, tvendor:parUse, ed_1or_more:ed_both_high,
         perc_vend_home_a_little:perc_vend_home_a_lot, 
         perc_vend_sch_a_little:perc_vend_sch_a_lot, w2_study_weight,
         num_hh_IV, store_visit_index) %>%
  format_data_mice

# How many people have some missing data?
sum(!complete.cases(mdata_all))
sum(!complete.cases(kdata_all))

impute_data = TRUE
if(impute_data == TRUE) {
  set.seed(7794)
  mdata_all = bind_cols(
    mdata_all %>% select(IV_Unit, UFSBlockNo, blocstrata, IVstrata2),
    complete(mice(mdata_all %>% select(!c(IV_Unit, UFSBlockNo, blocstrata, 
      IVstrata2)), m = 1))) %>% drop_na()
  kdata_all = bind_cols(
    kdata_all %>% select(IV_Unit, UFSBlockNo, blocstrata, IVstrata2),
    complete(mice(kdata_all %>% select(!c(IV_Unit, UFSBlockNo, blocstrata, 
      IVstrata2)), m = 1))) %>% drop_na()
} else {
  mdata_all = mdata_all %>% drop_na()
  kdata_all = kdata_all %>% drop_na()
}

# process and format the model output
process_model = function(x) {
  cbind(summary(x)$coefficients[,1], confint(x)) %>%
    exp() %>% signif(digits = 2)
}

formula_construct = function(response, rhs, adjusted) {
  if(adjusted == "full") {
    if(rhs == "vendor") {
      result = paste(c(response, "(1 | IV_Unit) + religionMuslim + genderChild + childage +
           ed_1or_more + ed_both_high + tvendor"), 
                     collapse = "~")
    } else if(rhs == "perc_home") {
      result = paste(c(response, "(1 | IV_Unit) + religionMuslim + genderChild + childage +
           ed_1or_more + ed_both_high + perc_vend_home_a_little + perc_vend_home_a_lot"), 
                     collapse = "~")
    } else if(rhs == "perc_school") {
      result = paste(c(response, "(1 | IV_Unit) + religionMuslim + genderChild + childage +
           ed_1or_more + ed_both_high + perc_vend_sch_a_little + perc_vend_sch_a_lot"), 
                     collapse = "~")
    } else if(rhs == "store_visit_index") {
      result = paste(c(response, "(1 | IV_Unit) + religionMuslim + genderChild + childage +
           ed_1or_more + ed_both_high + store_visit_index"), 
                     collapse = "~")
    }
  } else if(adjusted == "gender_age") {
    if(rhs == "vendor") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + tvendor"), 
                     collapse = "~")
    } else if(rhs == "perc_home") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + perc_vend_home_a_little + perc_vend_home_a_lot"), 
                     collapse = "~")
    } else if(rhs == "perc_school") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + perc_vend_sch_a_little + perc_vend_sch_a_lot"), 
                     collapse = "~")
    } else if(rhs == "store_visit_index") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + store_visit_index"), 
                     collapse = "~")
    } 
  } else if(adjusted == "gender_age_rel") {
    if(rhs == "vendor") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + religionMuslim + tvendor"), 
                     collapse = "~")
    } else if(rhs == "perc_home") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + religionMuslim + perc_vend_home_a_little + perc_vend_home_a_lot"), 
                     collapse = "~")
    } else if(rhs == "perc_school") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + religionMuslim + perc_vend_sch_a_little + perc_vend_sch_a_lot"), 
                     collapse = "~")
    } else if(rhs == "store_visit_index") {
      result = paste(c(response, "(1 | IV_Unit) + genderChild + childage + religionMuslim + store_visit_index"), 
                     collapse = "~")
    } 
  } else if(adjusted == "null") {
    
    result = paste(c(response, "(1 | IV_Unit)"), 
                     collapse = "~")
    
  }
  return(result)
}

calc_vif = function(response, rhs, data, adjusted) {
  result =
    glm(formula = formula_construct(response, rhs = rhs, adjusted = adjusted),
        family = binomial(link = "logit"), data = data)
  car::vif(result)
}

fit_survey = function(response, rhs, data, adjusted = FALSE) {
  result =
    svyglm(formula = formula_construct(response, rhs = rhs, adjusted = adjusted),
           family = binomial(link = "logit"),
           design = svydesign(ids = ~ IV_Unit + UFSBlockNo, 
                              strata = ~ IVstrata2 + blocstrata,
                              weights = ~ w2_study_weight,
                              data = data))
  #print(calc_vif(response, rhs = rhs, data = data, adjusted))
  process_model(result)
}

fit_survey_mixed = function(response, rhs, data, adjusted = "full") {
  result =
    lme4::glmer(formula_construct(response, rhs = rhs, adjusted = adjusted),
          family = binomial(link = "logit"), data = data,
          weights = w2_study_weight)
  print(summary(result))
  sresult = cbind(exp(summary(result)$coef[,1]), 
      exp(confint(result, method = "Wald"))[-1,])
  return(sresult)
}

# Regression models
# Percieved access, tertile vendor effect
fit_survey_mixed("perc_access", rhs = "vendor", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_access", rhs = "perc_home", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_access", rhs = "perc_school", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_access", rhs = "store_visit_index", data = mdata_all, adjusted = "gender_age_rel")

fit_survey_mixed("perc_access", rhs = "vendor", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_access", rhs = "perc_home", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_access", rhs = "perc_school", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_access", rhs = "store_visit_index", data = kdata_all, adjusted = "gender_age_rel")

# Perceived norms, tertile vendor effect
fit_survey_mixed("perc_norms", rhs = "vendor", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_norms", rhs = "perc_home", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_norms", rhs = "perc_school", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_norms", rhs = "store_visit_index", data = mdata_all, adjusted = "gender_age_rel")

fit_survey_mixed("perc_norms", rhs = "vendor", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_norms", rhs = "perc_home", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_norms", rhs = "perc_school", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("perc_norms", rhs = "store_visit_index", data = kdata_all, adjusted = "gender_age_rel")

# intention, tertile vendor effect
fit_survey_mixed("intention", rhs = "vendor", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("intention", rhs = "perc_home", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("intention", rhs = "perc_school", data = mdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("intention", rhs = "store_visit_index", data = mdata_all, adjusted = "gender_age_rel")

fit_survey_mixed("intention", rhs = "vendor", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("intention", rhs = "perc_home", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("intention", rhs = "perc_school", data = kdata_all, adjusted = "gender_age_rel")
fit_survey_mixed("intention", rhs = "store_visit_index", data = kdata_all, adjusted = "gender_age_rel")

get.stats = function(data, response, rhs, null.rhs) {
  modl = lme4::glmer(paste0(response, rhs),
                     family = binomial(link = "logit"), 
                     data = data, weights = w2_study_weight)
  
  null.modl = lme4::glmer(paste0(response, null.rhs),
                          family = binomial(link = "logit"), 
                          data = data, weights = w2_study_weight)
  
  result.modl = rep(0, 4)
  names(result.modl) = c("delta.BIC", "Deviance", "Df", "Deviance pval")
  
  av = as.data.frame(anova(modl, null.modl))
  
  result.modl["delta.BIC"] = av[,"BIC"][2] - av[,"BIC"][1]
  result.modl["Deviance"] = av[,"Chisq"][2]
  result.modl["Df"] = av[,"Df"][2]
  result.modl["Deviance pval"] = av[,"Pr(>Chisq)"][2]
  result.modl = round(result.modl, digits = 5)
  
  return(result.modl)
}

get.stats.city = function(mumbai, kolkata, response, rhs, null.rhs) {
  br = rbind(get.stats(data = mumbai, response, rhs, null.rhs),
                 get.stats(data = kolkata, response, rhs, null.rhs))
  rownames(br) = c("Mumbai", "Kolkata")
  return(br)
}

#resp = "perc_access"
#resp = "perc_norms"
resp = "intention"
#tack = ""
#tack = " + tvendor"
#tack =  "+ perc_vend_home_a_little + perc_vend_home_a_lot"
#tack = " + perc_vend_sch_a_little + perc_vend_sch_a_lot"
tack = " + store_visit_index"
rh = "~ (1 | IV_Unit) + religionMuslim + genderChild + childage"
null.rh = rh
#null.rh =  " ~ (1 | IV_Unit)"

get.stats.city(mumbai = mdata_all, kolkata = kdata_all, response = resp, 
    rhs = paste0(rh, tack), null.rhs = null.rh)
