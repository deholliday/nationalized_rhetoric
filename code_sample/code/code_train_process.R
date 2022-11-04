#### code_train_process.R                                                                                     ####
#### Splits lemmatized data into training/testing sets, preprocesses, and runs multiple STM to find optimal K ####

#### Data Import and Split ####
data_lemmatized = rio::import(here("data","data_lemmatized.rds"))
customStop = rio::import(here("data", "customStop.rds"))
set.seed(34)
data_split = initial_split(data_lemmatized, prop = .8)
data_train = training(data_split)
data_test  = testing(data_split)
rio::export(data_split, here("data","data_split.rds"))

#### Tidymodels Preprocessing ####
set.seed(308)
# Define Recipe
stm_rec_raw = recipe(fed_level ~ text + year_pooled, data = data_train) |> 
  step_tokenize(text) |> 
  step_stopwords(text) |> # Remove snowball stopwords
  step_stopwords(text, custom_stopword_source = customStop) |> 
  step_tokenfilter(text,
                   min_times = 3,
                   max_tokens = Inf) |> 
  step_untokenize(text) |> 
  prep(retain = T)

# Bake Recipe, Return DFM for STM
stm_pre = bake(stm_rec_raw, new_data = NULL) |> 
  mutate(text = as.character(text)) |> 
  rownames_to_column(var = "doc_id") |> 
  corpus(text_field = "text") |> 
  tokens() |> 
  dfm()

rio::export(stm_pre, here("data","pre_stm_dfm.rds"))

#### Evaluate best K ####
## WARNING: take significant processing power and about 1 hour of run-time ##
k_vec = seq(10,100,10)
stm_k = data_frame(K = k_vec) |> 
  mutate(topic_model = map(K, ~stm(stm_pre,
                                   K = .,
                                   prevalence = ~fed_level*year_pooled,
                                   init.type = "Spectral",
                                   seed = 309,
                                   verbose = F)))

rio::export(stm_k, here("data","stm_k_mods.rds"))

### K = 40 ###
stm_k40 = stm_k |> 
  filter(K == 40) |>
  pull(topic_model) %>%
  .[[1]]
rio::export(stm_k40, here("data","stm_k40.rds"))

### Compute summary statistics of STM models ###
set.seed(309)
heldout = make.heldout(stm_pre)

k_result = stm_k |> 
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, stm_pre),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, stm_pre),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

rio::export(k_result, here("data","stm_k_result.rds"))