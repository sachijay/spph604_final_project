
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
  group_by(
    cycle
  ) %>% 
  summarise(
    n = n(),
    across(
      .cols = !n,
      .fns = ~ sum(is.na(.x))
    )
  ) %>% 
  pivot_longer(
    cols = !cycle,
    names_to = "variable",
    values_to = "n"
  ) %>% 
  print(
    n = 100
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

survey_design_table1 <- survey_design

survey_design_table1$variables <- survey_design_table1$variables %>% 
  mutate(
    num_smoke_inside = num_smoke_inside %>% 
      as.factor(),
    across(
      .cols = c(relative_asthma, asthma_ed_visits_year, lung_cancer, smoked_100_life, num_smoke_inside),
      .fns = ~ fct_na_value_to_level(.x, level = "Missing")
    )
  )


tab1 <- tbl_svysummary(
  data = survey_design_table1,
  by = copd_or_others,
  include = c(has_insurance, age_years, relative_asthma, asthma_ed_visits_year, lung_cancer, smoked_100_life, num_smoke_inside),
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
  label = list(
    has_insurance ~ "Has insurance",
    age_years ~ "Age (years)",
    relative_asthma ~ "Close relative with asthma",
    asthma_ed_visits_year ~ "ED visits for asthma/past yr",
    lung_cancer ~ "Lung cancer",
    smoked_100_life ~ "Smoked more than 100 cigarettes/life",
    num_smoke_inside ~ "# of people who smoke inside"
  )
) %>% 
  add_overall(
    last = FALSE
  ) %>% 
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall**",
    stat_1 = "**Has COPD**",
    stat_2 = "**Doesn't have COPD**"
  )
