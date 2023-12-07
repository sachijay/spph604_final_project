
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
  "mdc" = "copd_or_others ~ has_insurance + age_years + sex + income_ratio + smoking_status + num_smoke_inside + have_diabetes", ## Modified disjunctive criterion
  "min_adj_set" = "copd_or_others ~ has_insurance + age_years + sex + income_ratio + have_diabetes" ## Minimal adjustment set
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
        as_tibble() |> 
        mutate(
          est_ci = paste0(`**OR**`, " (", `**95% CI**`, ")")
        )
      
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
        ) |> 
        as_tibble() |> 
        mutate(
          est_ci = miscr:::get_num1_num2_num3_txt(estimate, `2.5 %`, `97.5 %`, .output_digits = 2)
        )
      
      return(out)
      
    }
  )

names(mod_results_all_mi) <- paste0("est_mi_", names(mod_results_all_mi))



## Model diagnostics ####

### ROC ####

## ROC for complete data

gof_roc_all_complete <- mod_all_complete |> 
  lapply(
    function(mod){
      
      out <- WeightedROC::WeightedROC(
        guess = predict(mod, type = "response"),
        label = dat_analytic_no_miss$copd_or_others
      ) |> 
        as_tibble()
      
      return(out)
      
    }
  )

gof_roc_dat_all_complete <- gof_roc_all_complete |> 
  bind_rows(
    .id = "model"
  ) |> 
  mutate(
    model = fct_case_when(
      model == "crude" ~ "Crude",
      model == "mdc" ~ "MDC",
      model == "min_adj_set" ~ "Min. adj."
    )
  )


## Plot the ROC curves for complete data

gof_roc_plot_all_complete <- gof_roc_dat_all_complete |> 
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


### AUC ####

## AUC for complete data

gof_auc_all_complete <- gof_roc_all_complete |> 
  sapply(
    WeightedROC::WeightedAUC
  ) |> 
  as_tibble(
    rownames = "model"
  )


## AUC for multiple imputation

gof_auc_all_mi <- mod_all_mi |> 
  lapply(
    function(mod){
      
      out <- mod$mod |> 
        sapply(
          function(mod_imp_i){
            
            out <- WeightedROC::WeightedAUC(
              WeightedROC::WeightedROC(
                guess = predict(mod_imp_i, type = "response"),
                label = dat_analytic$copd_or_others
              )
            )
            
            return(out)
            
          }
        ) |> 
        as_tibble()
      
      return(out)
      
    }
  ) |> 
  bind_rows(
    .id = "model"
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


## AL for complete data

gof_al_all_complete <- mod_all_complete |> 
  sapply(
    function(mod){
      
      out <- al_gof(
        fit = mod, 
        data = dat_analytic_no_miss,
        psu = "psu", 
        strata = "stratum",
        weight = "interview_wt_adj"
      )
      
      return(out)
      
    }
  ) |> 
  as_tibble(
    rownames = "model"
  )


## AL for multiple imputation

gof_al_all_mi <- mod_all_mi |> 
  lapply(
    function(mod){
      
      out <- 1:n_imputations |> 
        sapply(
          function(imp_i){
            
            mod_imp_i <- mod$mod[[imp_i]]
            
            dat_imp_i <- imp_dat_full_list[[imp_i]] |> 
              filter(!exclude)
            
            out <- al_gof(
              fit = mod_imp_i, 
              data = dat_imp_i,
              psu = "psu", 
              strata = "stratum",
              weight = "interview_wt_adj"
            )
            
            return(out)
            
          }
        ) |> 
        as_tibble()
      
      return(out)
      
    }
  ) |> 
  bind_rows(
    .id = "model"
  )



## Save results ####

## ROC plot

ggsave(
  filename = here::here("figures", "roc_all_complete.png"),
  plot = gof_roc_plot_all_complete
)


## Other results

c(
  mod_results_all_complete,
  mod_results_all_mi,
  list(
    gof_auc_complete = gof_auc_all_complete,
    gof_auc_mi = gof_auc_all_mi,
    gof_al_complete = gof_al_all_complete,
    gof_al_mi = gof_al_all_mi
  )
) |> 
  writexl::write_xlsx(
    path = here::here("results", "mod_results.xlsx")
  )
  
