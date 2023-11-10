
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

model_base_crude <- svyglm(
  copd_or_others ~ has_insurance,
  design = survey_design,
  family = binomial(link = "logit")
)

model_base <- svyglm(
  copd_or_others ~ has_insurance + n_times_healthcare_visit + smoking_status + num_smoke_inside + have_diabetes + age_years + sex,
  design = survey_design,
  family = binomial(link = "logit")
)

result_mod_base <- tbl_regression(
  model_base,
  exponentiate = TRUE
)


## Model diagnostics ####

### ROC ####

gof_dat_unweighted_roc <- WeightedROC::WeightedROC(
  guess = predict(model_base_unweighted, type = "response"),
  label = dat_analytic_no_miss$copd_or_others
) %>% 
  as_tibble() %>% 
  add_column(
    model = "Unweighted",
    .before = 1
  )

gof_dat_weighted_roc <- WeightedROC::WeightedROC(
  guess = predict(model_base, type = "response"),
  label = dat_analytic_no_miss$copd_or_others,
  weight = dat_analytic_no_miss$interview_wt_adj
) %>% 
  as_tibble() %>% 
  add_column(
    model = "Weighted",
    .before = 1
  )

gof_dat_all_mod_roc <- bind_rows(
  gof_dat_unweighted_roc,
  gof_dat_weighted_roc
)


## Plot the ROC curves

gof_plot_all_mod <- gof_dat_all_mod_roc %>% 
  ggplot(
    aes(
      x = FPR,
      y = TPR,
      colour = model
    )
  ) + 
  geom_path() + 
  coord_equal() +
  labs(
    colour = "Model"
  ) +
  theme_classic()


### Area under the curve (AUC) ####

model_base_unweighted_auc <- WeightedROC::WeightedAUC(
  gof_dat_unweighted_roc
)

model_base_weighted_auc <- WeightedROC::WeightedAUC(
  gof_dat_weighted_roc
)


