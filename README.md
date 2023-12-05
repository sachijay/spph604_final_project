# Association between health insurance access and chronic obstructive pulmonary disease (COPD) among adults with asthma in the United States
 
Chronic obstructive pulmonary disease (COPD) is an irreversible respiratory disease which worsens over time. Over 15 million adults in the United States (U.S.) have reported a history of COPD, including bronchitis and emphysema. Chronic lower respiratory diseases, primarily COPD, is a leading cause of death in the U.S. The literature also shows that adults in the U.S. without health insurance, in general, have less access to recommended care, receive poorer quality of care, and experience worse health outcomes than adults covered by health insurance.

With a lack of universal healthcare in the U.S., it is questioned how patients manage chronic conditions in this complex system. To address this issue, this study focuses on the relationship between health insurance access and the likelihood of COPD in adults previously diagnosed with asthma. Asthma patients are selected for this study to avoid bias from misdiagnosis of asthma as COPD. This research aims to fill this literature gap by analyzing the general U.S. adult population from NHANES data from 2015 to 2020.

This study addresses the research question, "Is there an association between any type of insurance access and COPD among individuals with asthma in the U.S.?".


## File structure

- `R` source code in [`src`](src)  
    - [`0_main.r`](src/0_main.r) - Install and load necessary packages  
    - [`1_data_read.r`](src/1_data_read.r) - Read, clean and save the data required for the project. This file also includes code to perform imputation and use survey design features
    - [`2_sample_explore.r`](src/2_sample_explore.r) - Explore the sample and generate Table 1
    - [`3_run_model.r`](src/3_run_model.r) - Run the logistic regression model
    