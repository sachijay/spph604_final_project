
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
