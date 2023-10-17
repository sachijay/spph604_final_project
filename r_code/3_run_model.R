
## Read other scripts ####

source(
  file = here::here("r_code", "0_main.r")
)


## Read data ####

load(
  file = here::here("data", "final_data.rdata")
)


## Run model ####

### Unweighted ####

model_base_unweighted <- glm(
  copd_or_others ~ has_insurance + n_times_healthcare_visit + smoking_status + num_smoke_inside + have_diabetes + age_years + sex,
  data = dat_analytic,
  family = binomial(link = "logit")
)

result_mod_base_unweighted <- tbl_regression(
  model_base_unweighted,
  exponentiate = TRUE
)


### Weighted ####

model_base <- svyglm(
  copd_or_others ~ has_insurance + n_times_healthcare_visit + smoking_status + num_smoke_inside + have_diabetes + age_years + sex,
  design = survey_design,
  family = binomial(link = "logit")
)

result_mod_base <- tbl_regression(
  model_base,
  exponentiate = TRUE
)
