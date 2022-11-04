## code_stm_ads.R

#### Functions ####
process_fit = \(newdata, customStop, trained_mod, trained_cov){
  new_processed = textProcessor(
    newdata$text, newdata,
    customstopwords = customStop,
    wordLengths = c(4,Inf),
    stem = F)
  
  new_prepped = prepDocuments(new_processed$documents,
                              vocab = new_processed$vocab,
                              meta = new_processed$meta)
  
  new_aligned = alignCorpus(new_prepped, old.vocab = trained_mod$vocab)
  
  new_fitted = fitNewDocuments(model = trained_mod,
                               documents = new_aligned$documents,
                               newData = new_aligned$meta[,1:2],
                               origData = trained_cov,
                               prevalence = ~fed_level*year_pooled)
  return(list(mod_fitted = new_fitted,
              aligned = new_aligned))
}

#### Import Data ####

# Training Theta
theta_split = rio::import(here("data","theta_split.rds"))
theta_train = training(theta_split)

# Stopwords
customStop = rio::import(here("data","customStop.rds"))

# STM Models
stm_k40 = rio::import(here("data","stm_k40.rds"))

# Baked pre-STM DFM
stm_pre = rio::import(here("data","pre_stm_dfm.rds"))

# Lemmatize Ads
data_ads = rio::import(here("data","ads_output","gov_ads_clean.rds")) |> 
  bind_rows(rio::import(here("data","ads_output","pres_ads_clean.rds"))) |> 
  bind_rows(rio::import(here("data","ads_output","ads_cleaned_2004.rds"))) |> 
  mutate(text = str_squish(text)) |> 
  filter(text != "") |> 
  mutate(across(year_pooled:fed_level, as.factor)) |> 
  rownames_to_column(var = "doc_id")

ads_spacy = data_ads |>
  corpus(text_field = "text") |> 
  spacy_parse() |> 
  filter(pos != "PUNCT", pos != "SYM")

ads_lemmatized = ads_spacy |> 
  mutate(lemma = tolower(lemma)) |> 
  filter(nchar(lemma) > 3) |> 
  group_by(doc_id) |> 
  summarise(text = str_c(lemma, collapse = " ")) |> 
  ungroup() |> 
  left_join(select(data_ads, -text))

rio::export(ads_lemmatized, here("data","ads_output","ads_lemmatized.rds"))

# CSPAN -> Theta
ads_lemmatized = rio::import(here("data","ads_output","ads_lemmatized.rds"))
data_ads_p = ads_lemmatized |>
  select(doc_id, text, fed_level, year_pooled) |> 
  process_fit(customStop, stm_k40, stm_pre@docvars)

ads_test = as.data.frame(data_ads_p$mod_fitted$theta) |> 
  mutate(fed_level = as.factor(data_ads_p$aligned$meta$fed_level),
         year_pooled = data_ads_p$aligned$meta$year_pooled,
         set = "test")

rio::export(ads_test, here("data","ads_output","ads_test.rds"))

#### Create Split Objects with Theta DFs ####
ads_split = bind_rows(theta_train, ads_test) |> 
  mutate(.row = row_number())

indices = list(analysis   = ads_split$.row[ads_split$set == "train"],
               assessment = ads_split$.row[ads_split$set == "test"])

ads_split = make_splits(indices, ads_split |> select(-.row))

#### Fit Trained Models ####
trained_mods = rio::import(here("output","trained_mods.rds"))
log_ads_fit   = last_fit(extract_workflow(trained_mods$logistic), split = ads_split)
nb_ads_fit    = last_fit(extract_workflow(trained_mods$nb), split = ads_split)    # Naive Bayes
lasso_ads_fit = last_fit(extract_workflow(trained_mods$lasso), split = ads_split) # Lasso
xgb_ads_fit   = last_fit(extract_workflow(trained_mods$xgb), split = ads_split)   # XGBoost
svm_ads_fit   = last_fit(extract_workflow(trained_mods$svm), split = ads_split)   # SVM (Linear)

#### Save models ####
ads_mods = list(logistic = log_ads_fit,
                nb = nb_ads_fit,
                lasso = lasso_ads_fit,
                xgb = xgb_ads_fit,
                svm = svm_ads_fit)

rio::export(ads_mods, here("output","ads_mods.rds"))

rm(ads_lemmatized,ads_spacy,data_ads)
