README.txt

Instructions and descriptions of replication materials for Holliday (PSRM, 2023)

## Replication Instructions

1. Download all materials from dataverse as .zip
2. Unzip all items to folder holliday_2023_psrm
3. Create a new R Project under the existing directory holliday_2023_psrm
4. Run 00_master.R to replicate all analyses and figures

## Notes on Replication Materials

1. Initial scraping of C-SPAN transcripts took place in 2020. Website structure and output has likely
changed since the initial scrape. Code for scraping can be made available on demand
2. Ads data are embargoed by the Wisconsin Media Project. Code to replicate findings for ads are
available in the replication materials, but the data are not.

## File Descriptions

# Code
1. 00_master.R                - runs all .R files for replication of main analyses and figures
2. code_stopword_creation.R   - specifies custom stopwords
3. code_train_lemmatization.R - ingests and lemmatizes training data
4. code_train_process.R       - preprocesses lemmatized data, performs multiple STM for optimal K
5. code_stm_postprocess.R	- aligns validation data to trained STM, exports estimated thetas
6. code_stm_classification.R	- trains classification models

# Output
1. customStop.rds      - custom stopwords
2. data_lemmatized.rds - lemmatized training data
3. data_split.rds      - training data split into training and validation sets
4. pre_stm_dfm.rds     - document feature matrix of training data prior to STM
5. stm_k_mods.rds      - STM model output for K = 10 - 100 by 10
6. stm_k40.rds         - STM model output for K = 40
7. stm_k_result.rds    - Summary statistics for STM model outputs
8. theta_split.rds     - Thetas of training and validation data
9. trained_mods.rds    - Trained classification models