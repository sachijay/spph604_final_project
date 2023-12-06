
## Read other scripts ####

source(
  file = here::here("src", "0_main.r")
)


## Read data ####

load(
  file = here::here("data", "final_data.rdata")
)


## Run models ####

mod_formula <- c(
  "crude" = "copd_or_others ~ has_insurance", ## Crude
  "min_adj_set" = "copd_or_others ~ has_insurance + age_years + sex + income_ratio + smoking_status + num_smoke_inside + have_diabetes", ## Minimal adjustment set
  "mdc" = "copd_or_others ~ has_insurance + age_years + sex + income_ratio + have_diabetes" ## Modified disjunctive criterion
)


### Complete case ####

mod_all_complete <- mod_formula |> 
  lapply(
    function(form){
      
      out <- svyglm(
        as.formula(form),
        design = survey_design_no_miss,
        family = binomial(link = "logit")
      )
      
      return(out)
      
    }
  )


### Multiple imputation ####

n_imputations <- imp_dat_full_list |> 
  length() ## Added here, used later

mod_all_mi <- lapply(
  mod_formula,
  function(form){

    mod <- imp_survey_design_analytic_no_miss_list |>
      lapply(
        function(imp){

          mod_imp_i <- svyglm(
            as.formula(form),
            design = imp$analytic,
            family = binomial(link = "logit")
          )

          return(
            mod_imp_i
          )

        }
      )

    pooled_est <- mod |>
      mice::pool()

    out <- list(
      mod = mod,
      pooled_est = pooled_est
    )

    return(out)

  }
)


## Prepare output tables ####

## Complete data 

mod_results_all_complete <- mod_all_complete |> 
  lapply(
    function(mod){
      
      out <- tbl_regression(
        mod,
        exponentiate = TRUE
      ) |> 
        as_tibble()
      
    }
  )

names(mod_results_all_complete) <- paste0("est_complete_", names(mod_results_all_complete))


## Multiple imputation

mod_results_all_mi <- mod_all_mi |> 
  lapply(
    function(mod){
      
      out <- mod$pooled_est |> 
        summary(
          exponentiate = TRUE,
          conf.int = TRUE
        )
      
      return(out)
      
    }
  )

names(mod_results_all_mi) <- paste0("est_mi_", names(mod_results_all_mi))



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


1:n_imputations |> 
  sapply(
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

