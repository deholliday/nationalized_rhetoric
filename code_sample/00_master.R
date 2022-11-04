#### Master Script for Holliday (2022): Nationalized Elections, Localized Campaigns ####
#### This script runs all code and creates all figures                              ####

### Step 1: Install and Load Packages
#install.packages(c("tidyverse","tidymodels","here","rio",
#                   "quanteda","tidytext","stm","textrecipes",
#                   "themis","spacyr","discrim","doParallel","treemapify"))

# General purpose
library(tidyverse)
library(here)
# Text Analysis
library(quanteda)
library(tidytext)
library(stm)
library(textrecipes)
# Classification
library(tidymodels)
library(themis)
library(spacyr)
library(discrim)
# Plotting
library(kableExtra)
library(patchwork)
library(treemapify)

### Step 2: Ingest and Process Training Data
source(here("code","code_stopword_creation.R"))   # 2a: Stopword Specification
source(here("code","code_train_lemmatization.R")) # 2b: Lemmatize training data
source(here("code","code_train_process.R"))       # 2c: Process lemmatized training data

### Step 3: Classification Model
source(here("code","code_stm_postprocess.R"))    # 3a: Fit validation data to model, extract thetas
source(here("code","code_stm_classification.R")) # 3b: Train classification models

### Step 4: Apply Model to Corpuses
source(here("code","code_stm_cspan.R"))   # 4a CSPAN debates
#source(here("code","code_stm_ads.R"))    # 4b Advertisements (Embargoed)
source(here("code","code_stm_twitter.R")) # 4c Twitter (WARNING: significant processing power required)

### Step 5: Table Creation
source(here("code","table1_create.R"))

### Step 6: Figure Creation
source(here("code","fig1_create.R"))
source(here("code","fig2_create.R"))
source(here("code","fig3_create.R"))
source(here("code","fig4_create.R"))

### Step 7: Appendix Materials
source(here("code","fig_a1_1_create.R"))
source(here("code","code_appendix_kvalidation.R"))
source(here("code","table_a1_1_create.R"))
source(here("code","table_a2_create.R"))
