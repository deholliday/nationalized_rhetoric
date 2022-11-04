## code_stm_twitter.R

#### Functions ####
process_fit_twitter = \(newdata, customStop, trained_mod, trained_cov){
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
                               #newData = new_aligned$meta[,"text"],
                               origData = trained_cov,
                               prevalence = ~fed_level*year_pooled)
  return(list(mod_fitted = new_fitted,
              aligned = new_aligned))
}

#### Import Data ####

# Tweets
# NOTE: spacyr and reticulate don't get along well, so running their commands in separate
# R sessions may be necessary.
# NOTE: Tweets have already been lemmatized

detach("package:spacyr", unload = T)
library(reticulate)
#py_install("pandas") # if needed
source_python(here("code","pickle_reader.py"))
gub_pickle_data = read_pickle_file(here("data","twitter","governor_tweet_dict_4th_no_timelimit_lengthlimit"))
cng_pickle_data = read_pickle_file(here("data","twitter","congress_tweet_dict_4th_no_timelimit_lengthlimit"))

gub_pickle_tbl = map_dfr(names(gub_pickle_data), \(x){
  data.frame(handle = x,
             text = gub_pickle_data[[x]],
             fed_level = "state")
})
cng_pickle_tbl = map_dfr(names(cng_pickle_data), \(x){
  data.frame(handle = x,
             text = cng_pickle_data[[x]],
             fed_level = "national")
})

pickle_tbl = bind_rows(cng_pickle_tbl, gub_pickle_tbl)

rio::export(pickle_tbl, here("data","pickle_tweets.rds"))
pickle_tbl = rio::import(here("data","pickle_tweets.rds"))

# Training Theta
theta_split = rio::import(here("data","theta_split.rds"))
theta_train = training(theta_split)

# Stopwords
customStop = rio::import(here("data","customStop.rds"))

# STM Models
stm_k40 = rio::import(here("data","stm_k40.rds"))

# Baked pre-STM DFM
stm_pre = rio::import(here("data","pre_stm_dfm.rds"))

#### Tweets -> Theta ####
data_twitter = pickle_tbl |> 
  process_fit_twitter(customStop, stm_k40, stm_pre@docvars)

twitter_test = as.data.frame(data_twitter$mod_fitted$theta) |> 
  mutate(fed_level = as.factor(data_twitter$aligned$meta$fed_level),
         set = "test")

#### Create Split Objects with Theta DFs ####
twitter_split = bind_rows(theta_train, twitter_test) |> 
  mutate(.row = row_number())

indices = list(analysis   = twitter_split$.row[twitter_split$set == "train"],
               assessment = twitter_split$.row[twitter_split$set == "test"])

twitter_split = make_splits(indices, twitter_split |> select(-.row))
rio::export(twitter_split, here("data","twitter_splits.rds"))

#### Fit Trained Models ####
twitter_split = rio::import(here("data","twitter_splits.rds"))

#### Fit Trained Models ####
trained_mods = rio::import(here("output","trained_mods.rds"))
log_twitter_fit   = last_fit(extract_workflow(trained_mods$logistic), split = twitter_split)
nb_twitter_fit    = last_fit(extract_workflow(trained_mods$nb), split = twitter_split)    # Naive Bayes
lasso_twitter_fit = last_fit(extract_workflow(trained_mods$lasso), split = twitter_split) # Lasso
xgb_twitter_fit   = last_fit(extract_workflow(trained_mods$xgb), split = twitter_split)   # XGBoost
svm_twitter_fit   = last_fit(extract_workflow(trained_mods$svm), split = twitter_split)   # SVM (Linear)

#### Save models ####
twitter_mods = list(logistic = log_twitter_fit,
                    nb = nb_twitter_fit,
                    lasso = lasso_twitter_fit,
                    xgb = xgb_twitter_fit,
                    svm = svm_twitter_fit)

rio::export(twitter_mods, here("output","twitter_mods.rds"))