## code_stm_cspan.R

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


#### rio::import Data ####

# Training Theta
theta_split = rio::import(here("data","theta_split.rds"))
theta_train = training(theta_split)

# Stopwords
customStop = rio::import(here("data","customStop.rds"))

# STM K=40
stm_k40 = rio::import(here("data","stm_k40.rds"))

# Baked pre-STM DFM
stm_pre = rio::import(here("data","pre_stm_dfm.rds"))

#### Lemmatize CSPAN ####
cspan_spacy = rio::import(here("data","vid_prestm.rds")) |> 
  filter(text != "") |> 
  mutate(across(year_pooled:fed_level, as.factor)) |> 
  rownames_to_column(var = "doc_id") |> 
  corpus(text_field = "text") |> 
  spacy_parse() |> 
  filter(pos != "PUNCT", pos != "SYM")

cspan_lemmatized = cspan_spacy |> 
  mutate(lemma = tolower(lemma)) |> 
  filter(nchar(lemma) > 3) |> 
  group_by(doc_id) |> 
  summarise(text = str_c(lemma, collapse = " ")) |> 
  ungroup() |> 
  left_join(rio::import(here("data","vid_prestm.rds")) |> 
              filter(text != "") |> 
              mutate(across(year_pooled:fed_level, as.factor)) |> 
              rownames_to_column(var = "doc_id") |> 
              select(-text))

rio::export(cspan_lemmatized, here("data","cspan_lemmatized.rds"))
rm(cspan_spacy,cspan_lemmatized)

# CSPAN -> Theta
data_cspan = rio::import(here("data","cspan_lemmatized.rds")) |> 
  process_fit(customStop, stm_k40, stm_pre@docvars)

cspan_test = as.data.frame(data_cspan$mod_fitted$theta) |> 
  mutate(fed_level = as.factor(data_cspan$aligned$meta$fed_level),
         year_pooled = data_cspan$aligned$meta$year_pooled,
         set = "test")

rio::export(cspan_test, here("data","cspan_test.rds"))

#### Create Split Objects with Theta DFs ####
cspan_split = bind_rows(theta_train, cspan_test) |> 
  mutate(.row = row_number())

indices = list(analysis   = cspan_split$.row[cspan_split$set == "train"],
               assessment = cspan_split$.row[cspan_split$set == "test"])

cspan_split = make_splits(indices, cspan_split |> select(-.row))

#### Fit Trained Models ####

log_cspan_fit   = last_fit(log_fit, split = cspan_split)
nb_cspan_fit    = last_fit(nb_final, split = cspan_split)    # Naive Bayes
lasso_cspan_fit = last_fit(lasso_final, split = cspan_split) # Lasso
xgb_cspan_fit   = last_fit(xgb_final, split = cspan_split)   # XGBoost
svm_cspan_fit   = last_fit(svm_final, split = cspan_split)   # SVM (Linear)

#### Save models ####
cspan_mods = list(logistic = log_cspan_fit,
                  nb = nb_cspan_fit,
                  lasso = lasso_cspan_fit,
                  xgb = xgb_cspan_fit,
                  svm = svm_cspan_fit)



rio::export(cspan_mods, here("output","cspan_mods.rds"))