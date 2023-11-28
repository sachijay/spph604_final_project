
## Read other scripts ####

source(
  file = here::here("r_code", "0_main.r")
)


## Read data ####

load(
  file = here::here("data", "final_data.rdata")
)


## Run model ####

### Base - complete case ####

## Non-design adjusted
mod_base_design_unadjusted <- glm(
  copd_or_others ~ has_insurance + n_times_healthcare_visit + smoking_status + num_smoke_inside + have_diabetes + age_years + sex,
  data = dat_analytic_no_miss,
  family = binomial(link = "logit")
)


## Design adjusted
mod_base_design_adjusted_crude <- svyglm(
  copd_or_others ~ has_insurance,
  design = survey_design_no_miss,
  family = binomial(link = "logit")
)

mod_base_design_adjusted <- svyglm(
  copd_or_others ~ has_insurance + n_times_healthcare_visit + smoking_status + num_smoke_inside + have_diabetes + age_years + sex,
  design = survey_design_no_miss,
  family = binomial(link = "logit")
)


### Base - multiple imputation ####

## Design adjusted

mod_imp_survey_design_analytic_crude_list <- imp_survey_design_analytic_no_miss_list |> 
  lapply(
    function(imp){
      
      mod_imp_i_base_design_adjusted <- svyglm(
        copd_or_others ~ has_insurance,
        design = imp$analytic,
        family = binomial(link = "logit")
      )
      
      return(
        mod_imp_i_base_design_adjusted
      )
      
    }
  )

mod_imp_survey_design_adjusted_crude_pooled <- mod_imp_survey_design_analytic_crude_list |> 
  mice::pool()

mod_imp_survey_design_analytic_list <- imp_survey_design_analytic_no_miss_list |> 
  lapply(
    function(imp){
      
      mod_imp_i_base_design_adjusted <- svyglm(
        copd_or_others ~ has_insurance + n_times_healthcare_visit + smoking_status + num_smoke_inside + have_diabetes + age_years + sex,
        design = imp$analytic,
        family = binomial(link = "logit")
      )
      
      return(
        mod_imp_i_base_design_adjusted
      )
      
    }
  )

mod_imp_survey_design_adjusted_pooled <- mod_imp_survey_design_analytic_list |> 
  mice::pool()


## Prepare output tables ####

result_mod_base_design_unadjusted <- tbl_regression(
  mod_base_design_unadjusted,
  exponentiate = TRUE
)

result_mod_base_design_adjusted_crude <- tbl_regression(
  mod_base_design_adjusted_crude,
  exponentiate = TRUE
)

result_mod_base_design_adjusted <- tbl_regression(
  mod_base_design_adjusted,
  exponentiate = TRUE
)

mod_imp_survey_design_adjusted_crude_pooled |> 
  summary(
    exponentiate = TRUE,
    conf.int = TRUE
  )

mod_imp_survey_design_adjusted_pooled |> 
  summary(
    exponentiate = TRUE,
    conf.int = TRUE
  )


## Model diagnostics ####

### ROC ####

gof_mod_base_design_unadjusted_roc <- WeightedROC::WeightedROC(
  guess = predict(mod_base_design_unadjusted, type = "response"),
  label = dat_analytic_no_miss$copd_or_others
) |> 
  as_tibble() |> 
  add_column(
    model = "Unadjusted",
    .before = 1
  )

gof_mod_base_design_adjusted_crude_roc <- WeightedROC::WeightedROC(
  guess = predict(mod_base_design_adjusted_crude, type = "response"),
  label = dat_analytic_no_miss$copd_or_others,
  weight = dat_analytic_no_miss$interview_wt_adj
) |> 
  as_tibble() |> 
  add_column(
    model = "Adjusted - Crude",
    .before = 1
  )

gof_mod_base_design_adjusted_roc <- WeightedROC::WeightedROC(
  guess = predict(mod_base_design_adjusted, type = "response"),
  label = dat_analytic_no_miss$copd_or_others,
  weight = dat_analytic_no_miss$interview_wt_adj
) |> 
  as_tibble() |> 
  add_column(
    model = "Adjusted",
    .before = 1
  )

gof_dat_all_mod_roc <- bind_rows(
  gof_mod_base_design_adjusted_crude_roc,
  gof_mod_base_design_adjusted_roc
)


## Plot the ROC curves

gof_plot_all_mod <- gof_dat_all_mod_roc |> 
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

gof_mod_base_design_unadjusted_auc <- WeightedROC::WeightedAUC(
  gof_mod_base_design_unadjusted_roc
)

gof_mod_base_design_adjusted_crude_auc <- WeightedROC::WeightedAUC(
  gof_mod_base_design_adjusted_crude_roc
)

gof_mod_base_design_adjusted_auc <- WeightedROC::WeightedAUC(
  gof_mod_base_design_adjusted_roc
)


### Archer-Lemeshow goodness-of-fit test ####

## Note: This function is copied from https://ehsanx.github.io/EpiMethods/missingdata4.html
al_gof <- function(
    fit, data, 
    psu, strata, weight
){
  
  r <- residuals(fit, type = "response") 
  f <- fitted(fit)
  
  breaks.g <- c(-Inf, quantile(f, (1:9)/10), Inf)
  breaks.g <- breaks.g + seq_along(breaks.g)*.Machine$double.eps
  g <- cut(f, breaks.g)
  data2g <- cbind(data, r, g)
  
  if (is.null(psu)){
    
    newdesign <- svydesign(
      id = ~1,
      weights = as.formula(paste0("~", weight)),
      data = data2g, 
      nest = TRUE
    )
    
  } else {
    
    newdesign <- svydesign(
      id = as.formula(paste0("~", psu)),
      strata = as.formula(paste0("~", strata)),
      weights = as.formula(paste0("~", weight)),
      data = data2g, 
      nest = TRUE
    )
    
  }

  decilemodel <- svyglm(
    r ~ g, 
    design = newdesign
    )
  
  res <- survey::regTermTest(decilemodel, ~g)
  
  out <- as.numeric(res$p)
  
  return(out)
  
}


### For non-imputed data ####

gof_mod_base_design_unadjusted_al <- al_gof(
  fit = mod_base_design_unadjusted, 
  data = dat_analytic_no_miss,
  psu = "psu", 
  strata = "stratum",
  weight = "interview_wt_adj"
)

gof_mod_base_design_adjusted_crude_al <- al_gof(
  fit = mod_base_design_adjusted_crude, 
  data = dat_analytic_no_miss,
  psu = "psu", 
  strata = "stratum",
  weight = "interview_wt_adj"
)

gof_mod_base_design_adjusted_al <- al_gof(
  fit = mod_base_design_adjusted, 
  data = dat_analytic_no_miss,
  psu = "psu", 
  strata = "stratum",
  weight = "interview_wt_adj"
)


### For imputed data ####

n_imputations <- imp_dat_full_list |> 
  length()

1:n_imputations |> 
  lapply(
    function(imp_no){
      
      mod_fit_imp_no <- mod_imp_survey_design_analytic_list[[imp_no]]
      
      imp_dat_imp_no <- imp_dat_full_list[[imp_no]] |> 
        filter(
          !exclude
        )
      
      al_gof(
        fit = mod_fit_imp_no, 
        data = imp_dat_imp_no, 
        psu = "psu", 
        strata = "stratum",
        weight = "interview_wt_adj"
      )
      
    }
  )

