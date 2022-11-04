## code_stm_postprocess.R ##

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

# Stopwords
customStop = rio::import(here("data","customStop.rds"))

# Training/Testing
data_split = rio::import(here("data","data_split.rds"))
data_train = training(data_split)
data_test = testing(data_split)

# STM Models
#stm_k40 = import(here("output","stm_k_mods.rds")) |> 
#  filter(K == 40) |> # Grab K = 40 version, but 50 might also be good
#  pull(topic_model) %>%
#  .[[1]]
stm_k40 = rio::import(here("data","stm_k40.rds"))

# Baked pre-STM DFM
stm_pre = rio::import(here("data","pre_stm_dfm.rds"))

#### Fit Trained STM to Testing Document ####
stm_test = process_fit(data_test, customStop, stm_k40, stm_pre@docvars)

#### Extract Theta ####
theta_train = as.data.frame(stm_k40$theta) |> 
  bind_cols(stm_pre@docvars) |> 
  select(-segid_, -docid_) |> 
  mutate(set = "train")

theta_test = as.data.frame(stm_test$mod_fitted$theta) |> 
  bind_cols(stm_test$aligned$meta) |> 
  select(-text, -level_year) |> 
  rename(docname_ = doc_id) |> 
  mutate(set = "test")

#### Create Split Objects with Theta DFs ####
theta_split = bind_rows(theta_train, theta_test) |> 
  mutate(.row = row_number())

indices = list(analysis   = theta_split$.row[theta_split$set == "train"],
               assessment = theta_split$.row[theta_split$set == "test"])

theta_split = make_splits(indices, theta_split |> select(-.row))

rio::export(theta_split, here("data","theta_split.rds"))