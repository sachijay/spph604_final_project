
## Read other scripts ####

source(
  file = here::here("r_code", "0_main.r")
)


## Read data ####

load(
  file = here::here("data", "final_data.rdata")
)


## Run model ####

model_base <- svyglm(
  copd_or_others ~ has_insurance + age_years + relative_asthma + asthma_ed_visits_year + lung_cancer + smoked_100_life + num_smoke_inside,
  design = survey_design,
  family = binomial(link = "logit")
)

tbl_regression(
  model_base
)
