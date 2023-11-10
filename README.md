# Association between health insurance access and chronic obstructive pulmonary disease (COPD) among adults with asthma in the United States
 
Add introduction


## File structure

- Data files in [`data`](data)

- Exported figures in [`figures](figures)

- `R` source code in [`r_code`](r_code)  
    - [`0_main.r`](r_code/0_main.r) - Install and load necessary packages  
    - [`1_data_read.r`](r_code/1_data_read.r) - Read, clean and save the data required for the project. This file also includes code to perform imputation and use survey design features
    - [`2_sample_explore.r`](r_code/1_sample_explore.r) - Explore the sample and generate Table 1
    - [`3_run_model.r`](r_code/3_run_model.r) - Run the logistic regression model
    