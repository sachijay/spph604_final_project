
## Read other scripts ####

source(
  file = here::here("r_code", "0_main.r")
)


## Read data ####

load(
  file = here::here("data", "final_data.rdata")
)


## Explore missing values ####
## Note: Should tell if a variable is not matched at all 
##        between the cycles

dat_analytic %>% 
  get_n_missing_df() %>% 
  print(
    n = 25
  )


## Explore counts in other variables ####

dat_analytic %>%
  count(
    asthma
  )

dat_analytic %>%
  count(
    has_insurance
  )

dat_analytic %>%
  count(
    has_insurance
  )

dat_analytic %>%
  count(
    copd_or_others
  )

dat_analytic %>%
  count(
    copd_or_others
  )

dat_analytic %>%
  count(
    copd_or_others,
    has_insurance
  )


## Generate table 1 ####

### Ignoring sampling weights ####

dat_analytic_table1 <- dat_analytic %>% 
  mutate(
    num_smoke_inside = num_smoke_inside %>% 
      as.factor(),
    across(
      .cols = c(relative_asthma, asthma_ed_visits_year, lung_cancer, have_diabetes, health_care, smoking_status, num_smoke_inside),
      .fns = ~ fct_na_value_to_level(.x, level = "Missing")
    )
  ) %>% 
  labelled::set_variable_labels(
    num_smoke_inside = "No. of people who smoke inside"
  )

tab_1_unweighted <- tbl_summary(
  data = dat_analytic_table1,
  by = copd_or_others,
  include = c(has_insurance, age_years, sex, n_times_healthcare_visit, lung_cancer, smoking_status, num_smoke_inside, have_diabetes),
  percent = "column",
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{p}%"
  ),
  type = list(
    where(is.factor) ~ "categorical" ## To show both levels of dichotomous variables
  ),
  digits = list(
    all_continuous() ~ 1,
    all_categorical() ~ 0
  )
) %>% 
  add_overall(
    last = FALSE
  ) %>% 
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall** (n=2,154)",
    stat_1 = "**Has COPD** (n=508)",
    stat_2 = "**Doesn't have COPD** (n=1,646)"
  )


### Using sampling weights ####

survey_design_table1 <- survey_design

survey_design_table1$variables <- survey_design_table1$variables %>% 
  mutate(
    num_smoke_inside = num_smoke_inside %>% 
      as.factor(),
    across(
      .cols = c(relative_asthma, asthma_ed_visits_year, lung_cancer, have_diabetes, health_care, smoking_status, num_smoke_inside),
      .fns = ~ fct_na_value_to_level(.x, level = "Missing")
    )
  ) %>% 
  labelled::set_variable_labels(
    num_smoke_inside = "No. of people who smoke inside"
  )


tab_1_weighted <- tbl_svysummary(
  data = survey_design_table1,
  by = copd_or_others,
  include = c(has_insurance, age_years, sex, n_times_healthcare_visit, lung_cancer, smoking_status, num_smoke_inside, have_diabetes),
  percent = "column",
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{p}%"
  ),
  type = list(
    where(is.factor) ~ "categorical" ## To show both levels of dichotomous variables
  ),
  digits = list(
    all_continuous() ~ 1,
    all_categorical() ~ 0
  )
) %>% 
  add_overall(
    last = FALSE
  ) %>% 
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall** (n=2,154)",
    stat_1 = "**Has COPD** (n=508)",
    stat_2 = "**Doesn't have COPD** (n=1,646)"
  )
