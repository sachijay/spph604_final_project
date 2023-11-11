
## Read other scripts ####

source(
  file = here::here("r_code", "0_main.r")
)


## Read data ####

## Demographic
dat_15_demographic_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "DEMO_I.XPT")
)

dat_17_demographic_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_DEMO.XPT")
)


## Medical conditions

dat_15_medical_conditions_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "MCQ_I.XPT")
)

dat_17_medical_conditions_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_MCQ.XPT")
)


## Access to care

dat_15_access_to_care_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "HUQ_I.XPT")
)

dat_17_access_to_care_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_HUQ.XPT")
)


## Insurance

dat_15_insurance_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "HIQ_I.XPT")
)

dat_17_insurance_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_HIQ.XPT")
)


## Smoking (cigarette use)

dat_15_smoking_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "SMQ_I.XPT")
)

dat_17_smoking_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_SMQ.XPT")
)


## Smoking (household smokers)

dat_15_smoking_household_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "SMQFAM_I.XPT")
)

dat_17_smoking_household_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_SMQFAM.XPT")
)


## Diabetes

dat_15_diabetes_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "DIQ_I.XPT")
)

dat_17_diabetes_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_DIQ.XPT")
)


## Clean the data ####

### Demographic data ####
## Variables needed:
##    * SEQN - Respondent sequence number
##    * SDDSRVYR - Data release cycle
##    * RIAGENDR - Gender
##    * RIDAGEYR - Age in years at screening
##    * SDMVPSU - Pseudo PSU
##    * SDMVSTRA - Pseudo stratum
##    * WTINTPRP (2017-2020) or WTINT2YR (2015-2016) - Full sample 2 year interview weight
## 
## Notes: Ignored age - months

dat_15_demographic <- dat_15_demographic_raw %>% 
  select(
    subject_id = SEQN,
    cycle = SDDSRVYR,
    sex = RIAGENDR, 
    age_years = RIDAGEYR,
    psu = SDMVPSU,
    stratum = SDMVSTRA,
    interview_wt = WTINT2YR
  ) %>% 
  mutate(
    sex = fct_case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    )
  )

dat_17_demographic <- dat_17_demographic_raw %>% 
  select(
    subject_id = SEQN,
    cycle = SDDSRVYR,
    sex = RIAGENDR, 
    age_years = RIDAGEYR,
    psu = SDMVPSU,
    stratum = SDMVSTRA,
    interview_wt = WTINTPRP
  ) %>% 
  mutate(
    sex = fct_case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    )
  )


### Medical conditions ####
## Variables needed:
##    * SEQN - Respondent sequence number
##    * MCQ010 - Ever been told you have asthma (exclusion)
##    * MCQ160p (2017-2020) or MCQ160G/MCQ160K/MCQ160O (2015-2016) - Ever told you had COPD, emphysema, ChB (outcome)
##    * MCQ300B - Close relative had asthma?
##    * MCQ050 - Emergency care visit for asthma/past yr (for exposure)
##    * MCQ220 & MCQ230A/MCQ230B/MCQ230C/MCQ230D - What kind of cancer
dat_15_medical_conditions <- dat_15_medical_conditions_raw %>% 
  select(
    subject_id = SEQN,
    asthma = MCQ010,
    copd_1 = MCQ160G, copd_2 = MCQ160K, copd_3 = MCQ160O,
    relative_asthma = MCQ300B,
    asthma_ed_visits_year = MCQ050,
    cancer_diagnosis = MCQ220,
    cancer_type_1 = MCQ230A, cancer_type_2 = MCQ230B, cancer_type_3 = MCQ230C, cancer_type_4 = MCQ230D
  ) %>% 
  mutate(
    asthma = fct_case_when( ## Refused and don't know as NA
      asthma == 1 ~ "Yes", 
      asthma == 2 ~ "No"
    ),
    copd_or_others = fct_case_when( ## Refused and don't know as NA
      copd_1 == 1 | copd_2 == 1 | copd_3 == 1 ~ "Has COPD, emphysema, ChB",
      copd_1 == 2 & copd_2 == 2 & copd_3 == 2 ~ "Doesn't have COPD, emphysema, ChB"
    ),
    relative_asthma = miscr::fct_case_when( ## Refused and don't know as NA
      relative_asthma == 1 ~ "Yes",
      relative_asthma == 2 ~ "No"
    ), 
    lung_cancer = miscr::fct_case_when( ## Refused and don't know as NA
      cancer_type_1 == 23 | cancer_type_2 == 23 | cancer_type_3 == 23 | cancer_type_4 == 23 ~ "Yes", ## There are no lung cancers in 3 and 4, but added here for completeness
      cancer_diagnosis == 2 | !cancer_type_1 %in% c(23, 77, 99, NA) | !cancer_type_2 %in% c(23, 77, 99, NA) | !cancer_type_3 %in% c(23, 77, 99, NA) | !cancer_type_4 %in% c(23, 77, 99, NA) ~ "No",
      cancer_type_1 %in% c(77, 99, NA) & cancer_type_2 %in% c(77, 99, NA) & cancer_type_3 %in% c(77, 99, NA) & cancer_type_4 %in% c(77, 99, NA) ~ NA_character_
    ),
    asthma_ed_visits_year = miscr::fct_case_when( ## Refused and don't know as NA
      asthma_ed_visits_year == 1 ~ "Yes",
      asthma_ed_visits_year == 2 ~ "No"
    ), 
    .keep = "unused"
  )

dat_17_medical_conditions <- dat_17_medical_conditions_raw %>% 
  select(
    subject_id = SEQN,
    asthma = MCQ010,
    copd_or_others = MCQ160P,
    relative_asthma = MCQ300B,
    asthma_ed_visits_year = MCQ050,
    cancer_diagnosis = MCQ220,
    cancer_type_1 = MCQ230A, cancer_type_2 = MCQ230B, cancer_type_3 = MCQ230C, cancer_type_4 = MCQ230D
  ) %>% 
  mutate(
    asthma = fct_case_when( ## Refused and don't know as NA
      asthma == 1 ~ "Yes", 
      asthma == 2 ~ "No"
    ),
    copd_or_others = fct_case_when( ## Refused and don't know as NA
      copd_or_others == 1 ~ "Has COPD, emphysema, ChB",
      copd_or_others == 2 ~ "Doesn't have COPD, emphysema, ChB"
    ),
    relative_asthma = miscr::fct_case_when( ## Refused and don't know as NA
      relative_asthma == 1 ~ "Yes",
      relative_asthma == 2 ~ "No"
    ), 
    lung_cancer = miscr::fct_case_when( ## Refused and don't know as NA
      cancer_type_1 == 23 | cancer_type_2 == 23 | cancer_type_3 == 23 | cancer_type_4 == 23 ~ "Yes", ## There are no lung cancers in 3 and 4, but added here for completeness
      cancer_diagnosis == 2 | !cancer_type_1 %in% c(23, 77, 99, NA) | !cancer_type_2 %in% c(23, 77, 99, NA) | !cancer_type_3 %in% c(23, 77, 99, NA) | !cancer_type_4 %in% c(23, 77, 99, NA) ~ "No",
      cancer_type_1 %in% c(77, 99, NA) & cancer_type_2 %in% c(77, 99, NA) & cancer_type_3 %in% c(77, 99, NA) & cancer_type_4 %in% c(77, 99, NA) ~ NA_character_
    ),
    asthma_ed_visits_year = miscr::fct_case_when( ## Refused and don't know as NA
      asthma_ed_visits_year == 1 ~ "Yes",
      asthma_ed_visits_year == 2 ~ "No"
    ), 
    .keep = "unused"
  )


### Access to care ####
## Variables needed:
##    * SEQN - Respondent sequence number
##    * HUQ051 - # times receive healthcare over past year

dat_15_access_to_care <- dat_15_access_to_care_raw %>% 
  select(
    subject_id = SEQN,
    n_times_healthcare_visit = HUQ051
  ) %>% 
  mutate(
    health_care = fct_case_when( ## Refused and don't know as NA
      n_times_healthcare_visit == 0 ~ "None",
      n_times_healthcare_visit == 1 ~ "1",
      n_times_healthcare_visit == 2 ~ "2 to 3",
      n_times_healthcare_visit == 3 ~ "4 to 5",
      n_times_healthcare_visit == 4 ~ "6 to 7",
      n_times_healthcare_visit == 5 ~ "8 to 9",
      n_times_healthcare_visit == 6 ~ "10 to 12",
      n_times_healthcare_visit == 7 ~ "13 to 15",
      n_times_healthcare_visit == 8 ~ "16 or more"
    )
  )

dat_17_access_to_care <- dat_17_access_to_care_raw %>% 
  select(
    subject_id = SEQN,
    n_times_healthcare_visit = HUQ051
  ) %>% 
  mutate(
    health_care = fct_case_when( ## Refused and don't know as NA
      n_times_healthcare_visit == 0 ~ "None",
      n_times_healthcare_visit == 1 ~ "1",
      n_times_healthcare_visit == 2 ~ "2 to 3",
      n_times_healthcare_visit == 3 ~ "4 to 5",
      n_times_healthcare_visit == 4 ~ "6 to 7",
      n_times_healthcare_visit == 5 ~ "8 to 9",
      n_times_healthcare_visit == 6 ~ "10 to 12",
      n_times_healthcare_visit == 7 ~ "13 to 15",
      n_times_healthcare_visit == 8 ~ "16 or more"
    )
  )


### Insurance ####
## Variables needed:
##    * SEQN - Respondent sequence number
##    * HIQ011 - Covered by health insurance

dat_15_insurance <- dat_15_insurance_raw %>% 
  select(
    subject_id = SEQN,
    has_insurance = HIQ011
  ) %>% 
  mutate(
    has_insurance = fct_case_when( ## Refused and don't know as NA
      has_insurance == 1 ~ "Yes",
      has_insurance == 2 ~ "No"
    )
  )

dat_17_insurance <- dat_17_insurance_raw %>% 
  select(
    subject_id = SEQN,
    has_insurance = HIQ011
  ) %>% 
  mutate(
    has_insurance = fct_case_when( ## Refused and don't know as NA
      has_insurance == 1 ~ "Yes",
      has_insurance == 2 ~ "No"
    )
  )


### Smoking ####
## Variables needed:
##    * SEQN - Respondent sequence number
##    * SMQ020 - Smoked at least 100 cigarettes in life (from cigarette use)
##    * SMQ040 - Do you now smoke cigarettes?
##    * SMD470 - # of people who smoke inside this home? (from household smokers)

dat_15_smoking <- full_join(
  dat_15_smoking_raw %>% 
    select(
      subject_id = SEQN,
      smoked_100_life = SMQ020,
      smoking_now = SMQ040
    ),
  dat_15_smoking_household_raw %>% 
    select(
      subject_id = SEQN,
      num_smoke_inside = SMD470
    ),
  by = "subject_id"
) %>% 
  mutate(
    smoking_status = fct_case_when( ## Refused and don't know as NA
      smoking_now %in% c(1, 2) ~ "Current smokers",
      smoked_100_life == 1 & smoking_now == 3 ~ "Former smokers",
      smoked_100_life == 2 ~ "Never smoked"
    ),
    num_smoke_inside = case_when(
      num_smoke_inside %in% c(777, 999) ~ NA_integer_, ## Refused and don't know as NA
      TRUE ~ as.integer(num_smoke_inside)
    ),
    .keep = "unused"
  )

dat_17_smoking <- full_join(
  dat_17_smoking_raw %>% 
    select(
      subject_id = SEQN,
      smoked_100_life = SMQ020,
      smoking_now = SMQ040
    ),
  dat_17_smoking_household_raw %>% 
    select(
      subject_id = SEQN,
      num_smoke_inside = SMD470
    ),
  by = "subject_id"
) %>% 
  mutate(
    smoking_status = fct_case_when( ## Refused and don't know as NA
      smoking_now %in% c(1, 2) ~ "Current smokers",
      smoked_100_life == 1 & smoking_now == 3 ~ "Former smokers",
      smoked_100_life == 2 ~ "Never smoked"
    ),
    num_smoke_inside = case_when(
      num_smoke_inside %in% c(777, 999) ~ NA_integer_, ## Refused and don't know as NA
      TRUE ~ as.integer(num_smoke_inside)
    ),
    .keep = "unused"
  )


## Diabetes
## Variables needed:
##    * SEQN - Respondent sequence number
##    * DIQ010 - Doctor told you have diabetes

dat_15_diabetes <- dat_15_diabetes_raw %>% 
  select(
    subject_id = SEQN,
    have_diabetes = DIQ010
  ) %>% 
  mutate(
    have_diabetes = fct_case_when( ## Refused and don't know as NA
      have_diabetes == 1 ~ "Yes",
      have_diabetes == 2 ~ "No",
      have_diabetes == 3 ~ "Borderline"
    )
  )

dat_17_diabetes <- dat_17_diabetes_raw %>% 
  select(
    subject_id = SEQN,
    have_diabetes = DIQ010
  ) %>% 
  mutate(
    have_diabetes = fct_case_when( ## Refused and don't know as NA
      have_diabetes == 1 ~ "Yes",
      have_diabetes == 2 ~ "No",
      have_diabetes == 3 ~ "Borderline"
    )
  )


## Combine the data into a single dataset ####

dat_15 <- dat_15_demographic %>% 
  full_join(
    dat_15_medical_conditions,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_15_access_to_care,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_15_insurance,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_15_diabetes,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_15_smoking,
    by = "subject_id"
  ) %>% 
  mutate(
    interview_wt_adj = 2*interview_wt/5.2
  )

dat_17 <- dat_17_demographic %>% 
  full_join(
    dat_17_medical_conditions,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_17_access_to_care,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_17_insurance,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_17_diabetes,
    by = "subject_id"
  ) %>% 
  full_join(
    dat_17_smoking,
    by = "subject_id"
  )

dat_combined <- bind_rows(
  dat_15,
  dat_17
) %>% 
  mutate(
    interview_wt_adj = 3*interview_wt/5.2
  )


## Apply exclusion criteria ####

dat_full <- dat_combined %>% 
  mutate(
    asthma_inclusion = asthma == "Yes",
    age_inclusion = age_years >= 20 & age_years < 80, 
    exposure_na_inclusion = !is.na(has_insurance), 
    response_na_inclusion = !is.na(copd_or_others),
    exclude = !(asthma_inclusion & age_inclusion & 
                  exposure_na_inclusion & response_na_inclusion)
  )

## Not used when using survey designs
dat_analytic <- dat_full %>% ## 25,531 records
  filter(
    asthma_inclusion, ## 3,765 records remaining
    age_inclusion, ## 2,172 records remaining
    exposure_na_inclusion, ## 2,164 records remaining
    response_na_inclusion ## 2,154 records remaining
  ) %>%
  droplevels.data.frame()


## Complete data
dat_analytic_no_miss <- dat_analytic %>% 
  drop_na(
    copd_or_others, has_insurance, n_times_healthcare_visit, smoking_status, num_smoke_inside, have_diabetes, age_years, sex
  ) %>%
  droplevels.data.frame()

dat_full <- dat_full %>% 
  mutate(
    is_complete = subject_id %in% dat_analytic_no_miss$subject_id
  )


## Define the survey design ####

survey_design_full <- svydesign(
  id = ~psu, 
  strata = ~stratum, 
  weights = ~interview_wt,
  data = dat_full, 
  nest = TRUE
)


## Subset the survey design

survey_design_analytic <- subset(
  survey_design_full, 
  !exclude
)

survey_design_no_miss <- subset(
  survey_design_full, 
  is_complete
)

## Change variable labels
dat_analytic <- dat_analytic %>% 
  labelled::set_variable_labels(
    copd_or_others = "Have/Had COPD",
    has_insurance = "Has insurance",
    age_years = "Age (years)",
    sex = "Sex",
    relative_asthma = "Close relative with asthma",
    asthma_ed_visits_year = "ED visits for asthma/past yr",
    n_times_healthcare_visit = "No. of healthcare visits",
    lung_cancer = "Lung cancer",
    smoking_status = "Smoking status",
    num_smoke_inside = "No. of people who smoke inside",
    have_diabetes = "Has diabetes"
  )

dat_analytic_no_miss <- dat_analytic_no_miss %>% 
  labelled::set_variable_labels(
    copd_or_others = "Have/Had COPD",
    has_insurance = "Has insurance",
    age_years = "Age (years)",
    sex = "Sex",
    relative_asthma = "Close relative with asthma",
    asthma_ed_visits_year = "ED visits for asthma/past yr",
    n_times_healthcare_visit = "No. of healthcare visits",
    lung_cancer = "Lung cancer",
    smoking_status = "Smoking status",
    num_smoke_inside = "No. of people who smoke inside",
    have_diabetes = "Has diabetes"
  )

survey_design_analytic$variables <- survey_design_analytic$variables %>% 
  labelled::set_variable_labels(
    copd_or_others = "Have/Had COPD",
    has_insurance = "Has insurance",
    age_years = "Age (years)",
    sex = "Sex",
    relative_asthma = "Close relative with asthma",
    asthma_ed_visits_year = "ED visits for asthma/past yr",
    n_times_healthcare_visit = "No. of healthcare visits",
    lung_cancer = "Lung cancer",
    smoking_status = "Smoking status",
    num_smoke_inside = "No. of people who smoke inside",
    have_diabetes = "Has diabetes"
  )

survey_design_no_miss$variables <- survey_design_no_miss$variables %>% 
  labelled::set_variable_labels(
    copd_or_others = "Have/Had COPD",
    has_insurance = "Has insurance",
    age_years = "Age (years)",
    sex = "Sex",
    relative_asthma = "Close relative with asthma",
    asthma_ed_visits_year = "ED visits for asthma/past yr",
    n_times_healthcare_visit = "No. of healthcare visits",
    lung_cancer = "Lung cancer",
    smoking_status = "Smoking status",
    num_smoke_inside = "No. of people who smoke inside",
    have_diabetes = "Has diabetes"
  )


## Save the final data sets into a file ####

save(
  dat_full,
  dat_analytic,
  dat_analytic_no_miss,
  survey_design_full,
  survey_design_analytic,
  survey_design_no_miss,
  file = here::here("data", "final_data.rdata")
)
