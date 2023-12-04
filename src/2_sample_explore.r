
## Read other scripts ####

source(
  file = here::here("src", "0_main.r")
)


## Read data ####

load(
  file = here::here("data", "final_data.rdata")
)


## Explore missing values ####
## Note: Should tell if a variable is not matched at all 
##        between the cycles

dat_analytic |> 
  get_n_missing_df() |> 
  print(
    n = 25
  )


## Explore counts in other variables ####

dat_analytic |>
  count(
    asthma
  )

dat_analytic |>
  count(
    has_insurance
  )

dat_analytic |>
  count(
    has_insurance
  )

dat_analytic |>
  count(
    copd_or_others
  )

dat_analytic |>
  count(
    copd_or_others
  )

dat_analytic |>
  count(
    copd_or_others,
    has_insurance
  )


## Generate table 1 ####

### Ignoring sampling weights ####

dat_analytic_table1 <- dat_analytic |> 
  mutate(
    num_smoke_inside = num_smoke_inside |> 
      as.factor(),
  ) |> 
  labelled::set_variable_labels(
    num_smoke_inside = "No. of people who smoke inside"
  )

tab_1_design_unadjusted <- tbl_summary(
  data = dat_analytic_table1,
  by = copd_or_others,
  include = c(has_insurance, age_years, sex, n_times_healthcare_visit, lung_cancer, smoking_status, num_smoke_inside, have_diabetes),
  percent = "column",
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  type = list(
    where(is.factor) ~ "categorical" ## To show both levels of dichotomous variables
  ),
  digits = list(
    all_continuous() ~ 1,
    all_categorical() ~ 0
  ),
  missing = "ifany",
  missing_text = "Missing"
) |> 
  add_overall(
    last = FALSE
  ) |> 
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall** (n=2,154)",
    stat_1 = "**Has COPD** (n=508)",
    stat_2 = "**Doesn't have COPD** (n=1,646)"
  )


### Using sampling weights ####

survey_design_table1 <- survey_design_analytic

survey_design_table1$variables <- survey_design_table1$variables |> 
  mutate(
    num_smoke_inside = num_smoke_inside |> 
      as.factor(),
  ) |> 
  labelled::set_variable_labels(
    num_smoke_inside = "No. of people who smoke inside"
  )


tab_1_design_adjusted <- tbl_svysummary(
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
  ),
  missing = "ifany",
  missing_text = "Missing"
) |> 
  add_overall(
    last = FALSE
  ) |> 
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall** (n=2,154)",
    stat_1 = "**Has COPD** (n=508)",
    stat_2 = "**Doesn't have COPD** (n=1,646)"
  )


## Save table 1 to file ####

list(
  unadjusted = tab_1_design_unadjusted |> 
    as_tibble(),
  adjusted = tab_1_design_adjusted |> 
    as_tibble()
) |> 
  writexl::write_xlsx(
    path = here::here("results", "table_1.xlsx")
  )
