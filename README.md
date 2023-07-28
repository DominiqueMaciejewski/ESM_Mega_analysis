# Code and data for "Beyond main effects? Affect level as a moderator in the relation between affect dynamics and depressive symptoms"

This is the github of the manuscript "Beyond main effects? Affect level as a moderator in the relation between affect dynamics and depressive symptoms". 

In the folder scripts, you can find the code and associated output. In the folder data, you can find the datasets used in the analyses. 
The data were primarily uploaded for reproducing the results. If you want to use the data for a paper, I would appreciate if you send me an e-mail about it.

If you have any questions, anything is unclear and/or you find a mistake, please do not hesitate to contact me under <d.f.maciejewski@tilburguniversity.edu>.

## Folder: Code
In the folder code, you can find the Rscripts (.Rmd files) plus the associated output (html files).
One disclaimer: I am not a data scientist and do not work in R for such a long time. That also means that some parts of the code could have been shorter.

| Filename |	Description |
| ------------- | ------------- |
| Code_Beyond-Main-Effects_Data-preparation-for-OSF |	Data preparation of the datasets as I received them from co-authors. Here, I primarily made sure that all datasets got the same structure (long-format), checked for inconsistencies in sex and age (e.g., removed those that were older than 30 years old), cleaned possible faulty beeps (e.g., when only 5 were supposed to be sent, but there were 10 within a day for a person). The resulting cleaned datasets end with “_processed” and are in the folder: Processed data. Note, that I did not put the raw datasets as received from the co-authors on OSF, because they contained additional variables that were not central to this paper as well as sensitive information.|
| Code_Beyond-Main-Effects_Main-analyses |	Main analyses of the paper. They include the data preparation per dataset, the mega-analyses as well as other useful information (e.g., item wording).|
| Code_Beyond-Main-Effects_Sensitivity1_ComplianceThreshold50per |	Sensitivity analyses where analyses of the significant interaction effects were repeated while excluding individuals with a compliance below 50%.|
| Code_Beyond-Main-Effects_Sensitivity1_ComplianceThresholdnoexc |	Sensitivity analyses where analyses of the significant interaction effects were repeated while not excluding any individuals based on compliance.|
| Code_Beyond-Main-Effects_Sensitivity2_mis_bet |	Sensitivity analyses where analyses of the significant interaction effects were repeated where missing data-points within days/weeks were not removed for the affect dynamic calculations. |

## Folder: Data 
### Subfolder: 2_processed_data_for_OSF

This folder refers to the processed dataset per study. See Code_Beyond-Main-Effects_Data-preparation-for-OSF for what was done exactly.

| Filename	| Description| 
| ------------- | ------------- |
| dataset1_RADAR_processed	| Processed data of Dataset 1 (RADAR)| 
| dataset2_SM_processed	| Processed data of Dataset 2 (Swinging Moods)| 
| dataset3_MA_processed	| Processed data of Dataset 3 (Mood in Emerging Adults)| 
| dataset4_ED_processed	| Processed data of Dataset 4 (Emotions in Daily Life)| 
| dataset5_EA_processed | 	Processed data of Dataset 5 (Emotion Regulation in Action)| 
| dataset6_LASER_processed	|  Processed data of Dataset 6 (LASER)| 
| dataset7_YES_processed	| Processed data of Dataset 7 (YES)| 

### Subfolder: 3_cleaned_data_for_OSF
This folder contains the cleaned datasets per study (e.g., affect dynamics were calculated here). 
There are two versions. One with predictors mean-centered and depressive symptoms standardized within studies and one where predictors are not mean-centered and depressive symptoms are not standardized. 
The centered and standardized versions were used for correlation and regression analyses. 
The non-centered and non-standardized versions were used for descriptive statistics. 

See section 6 – DATAPREPARATION PER DATASET in code Code_Beyond-Main-Effects_Main-analyses for more information.

| Filename | 	Description | 
| ------------- | ------------- |
| dataset1_RADAR_clean | 	Cleaned data of Dataset 1 (RADAR) – no centering and standardization | 
| dataset2_SM_ clean|  Cleaned data of Dataset 2 (Swinging Moods) – no centering and standardization| 
| dataset3_MA_clean	| Cleaned data of Dataset 3 (Mood in Emerging Adults) – no centering and standardization| 
| dataset4_ED_clean	| Cleaned data of Dataset 4 (Emotions in Daily Life) – no centering and standardization| 
| dataset5_EA_clean	| Cleaned data of Dataset 5 (Emotion Regulation in Action) – no centering and standardization| 
| dataset6_LASER_clean	| Cleaned data of Dataset 6 (LASER) – no centering and standardization| 
| dataset7_YES_clean	| Cleaned data of Dataset 7 (YES) – no centering and standardization| 
| dataset1_RADAR_clean_cen_std| 	Cleaned data of Dataset 1 (RADAR) – predictors centered and outcomes standardized | 
| dataset2_SM_ clean_cen_std	| Cleaned data of Dataset 2 (Swinging Moods) – predictors centered and outcomes standardized| 
| dataset3_MA_clean_cen_std	| Cleaned data of Dataset 3 (Mood in Emerging Adults) – predictors centered and outcomes standardized| 
| dataset4_ED_clean_cen_std	| Cleaned data of Dataset 4 (Emotions in Daily Life) – predictors centered and outcomes standardized| 
| dataset5_EA_clean_cen_std	| Cleaned data of Dataset 5 (Emotion Regulation in Action) – predictors centered and outcomes standardized| 
| dataset6_LASER_clean_cen_std	| Cleaned data of Dataset 6 (LASER) – predictors centered and outcomes standardized| 
| dataset7_YES_clean_cen_std	| Cleaned data of Dataset 7 (YES) – predictors centered and outcomes standardized| 
| dataset_mega_all	| Pooled cleaned data of all datasets (dataset 1 to 7) – no centering and standardization| 
| dataset_mega_all_5	| Pooled cleaned data of datasets 1 to 5 (excluding datasets 6 and 7) – no centering and standardization| 
| dataset_mega_all_cen_std| 	Pooled cleaned data of all datasets (dataset 1 to 7) – predictors centered and outcomes standardized| 
| dataset_mega_all_5_cen_std| 	Pooled cleaned data of datasets 1 to 5 (excluding datasets 6 and 7)  – predictors centered and outcomes standardized| 

Happy reproducing!
