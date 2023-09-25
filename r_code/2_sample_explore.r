
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

dat_combined %>% 
  group_by(
    cycle
  ) %>% 
  summarise(
    n = n(),
    across(
      .cols = !n,
      .fns = ~ sum(is.na(.x))
    )
  )


## Explore counts in other variables ####

dat_combined %>% 
  count(
    asthma
  )

dat_combined %>% 
  count(
    has_insurance
  )

dat %>% 
  count(
    has_insurance
  )

dat_combined %>% 
  count(
    copd_or_others
  )

dat %>% 
  count(
    copd_or_others
  )

dat %>% 
  count(
    copd_or_others,
    has_insurance
  )


## Generate table 1 ####

dat_table_1 <- dat %>% 
  labelled::set_variable_labels(
    copd_or_others = "Have/Had COPD, emphysema, ChB",
    has_insurance = "Has insurance",
    age_years = "Age (years)",
    relative_asthma = "Close relative with asthma",
    asthma_ed_visits_year = "ED visits for asthma/past yr",
    lung_cancer = "Lung cancer",
    smoked_100_life = "Smoked more than 100 cigarettes/life",
    num_smoke_inside = "# of people who smoke inside"
  )

table1::table1(
  ~ has_insurance + age_years + relative_asthma + asthma_ed_visits_year + lung_cancer + smoked_100_life + num_smoke_inside | copd_or_others,
  data = dat_table_1,
  overall = c(left = "Total"),
  footnote = "ChB = Chronic bronchitis"
)

table_1 <- tableone::CreateTableOne(
  vars = c("has_insurance", "age_years", "relative_asthma", "asthma_ed_visits_year", "lung_cancer", "smoked_100_life", "num_smoke_inside"),
  strata = "copd_or_others",
  data = dat_table_1,
  addOverall = TRUE,
  includeNA = TRUE, 
  test = FALSE
)

print(
  table_1, 
  showAllLevels = TRUE, 
  varLabels = TRUE,
  
)

