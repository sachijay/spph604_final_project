
## Read other scripts ####

source(
  file = here::here("r_code", "0_main.r")
)


## Read data ####

dat_15_demographic_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "DEMO_I.XPT")
)

dat_15_medical_conditions_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "MCQ_I.XPT")
)

dat_15_access_to_care_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "HUQ_I.XPT")
)

dat_15_insurance_raw <- haven::read_xpt(
  file = here::here("data", "2015-2016", "HIQ_I.XPT")
)

dat_17_demographic_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_DEMO.XPT")
)

dat_17_medical_conditions_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_MCQ.XPT")
)

dat_17_access_to_care_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_HUQ.XPT")
)

dat_17_insurance_raw <- haven::read_xpt(
  file = here::here("data", "2017-2020", "P_HIQ.XPT")
)


## Clean the data ####

### Demographic data ####
## Notes: Ignored age - months

dat_15_demographic <- dat_15_demographic_raw %>% 
  select(
    id = SEQN,
    sex = RIAGENDR, 
    age_years = RIDAGEYR,
    interview_wt = WTINT2YR
  )

dat_17_demographic <- dat_17_demographic_raw %>% 
  select(
    id = SEQN,
    sex = RIAGENDR, 
    age_years = RIDAGEYR,
    interview_wt = WTINTPRP
  )


### Medical conditions ####

dat_15_medical_conditions <- dat_15_medical_conditions_raw

dat_17_medical_conditions <- dat_17_medical_conditions_raw


### Access to care ####

dat_15_access_to_care <- dat_15_access_to_care_raw

dat_17_access_to_care <- dat_17_access_to_care_raw


### Insurance ####

dat_15_insurance <- dat_15_insurance_raw

dat_17_insurance <- dat_17_insurance_raw


