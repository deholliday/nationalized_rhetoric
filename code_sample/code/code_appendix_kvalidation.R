#### Import Data ####

# Stopwords
customStop = rio::import(here("data","customStop.rds"))

# Training/Testing
data_split = rio::import(here("data","data_split.rds"))
data_train = training(data_split)
data_test = testing(data_split)

stm_k = rio::import(here("data","stm_k_mods.rds"))
stm_pre = rio::import(here("data","pre_stm_dfm.rds"))

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

#### Process and Train Models on Different K

for(i in seq(10,100,10)){
  stm_n = stm_k |> 
    filter(K == i) |>
    pull(topic_model) %>%
    .[[1]]
  
  stm_test = process_fit(data_test, customStop, stm_n, stm_pre@docvars)
  
  #### Extract Theta ####
  theta_train = as.data.frame(stm_n$theta) |> 
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
  theta_train = training(theta_split)
  theta_test = testing(theta_split)
  
  #### Create Folds for CV ####
  set.seed(310)
  folds = vfold_cv(theta_train, strata = fed_level)
  
  #### General Recipe ####
  gen_rec = recipe(fed_level ~ ., data = theta_train) |> 
    update_role(docname_, new_role = "id variable") |> 
    step_rm(year_pooled, set)
  
  rose_rec = gen_rec |> 
    step_rose(fed_level)
  
  #### Logistic Regression
  print("Performing Logistic Classification")
  log_spec = logistic_reg() |> 
    set_engine("glm") |> 
    set_mode("classification")
  
  log_wf = workflow() |> 
    add_recipe(rose_rec) |> 
    add_model(log_spec)
  
  log_fit = fit(log_wf, data = theta_train)
  log_final_fit = last_fit(log_fit, theta_split)
  
  #### Naive Bayes ####
  print("Performing Naive Classification")
  nb_spec = naive_Bayes(Laplace = tune()) |> 
    set_mode("classification") |> 
    set_engine("naivebayes")
  
  nb_wf = workflow() |> 
    add_recipe(rose_rec) |> 
    add_model(nb_spec)
  
  nb_grid = expand.grid(Laplace = 0:5)
  
  set.seed(345)
  nb_tune = tune_grid(nb_wf,
                      resamples = folds,
                      grid = nb_grid,
                      control = control_grid(save_pred = TRUE))
  
  nb_best = select_best(nb_tune, "roc_auc")
  nb_final = finalize_workflow(nb_wf, nb_best)
  nb_final_fit = last_fit(nb_final, split = theta_split)
  
  #### Lasso ####
  print("Performing Lasso Classification")
  lasso_spec = logistic_reg(penalty = tune(), mixture = 1) %>%
    set_mode("classification") %>%
    set_engine("glmnet")
  
  lasso_wf = workflow() |> 
    add_recipe(rose_rec) |> 
    add_model(lasso_spec)
  
  lambda_grid = grid_regular(penalty(), levels = 30)
  
  set.seed(2022)
  lasso_tune = tune_grid(lasso_wf,
                         resamples = folds,
                         grid = lambda_grid,
                         control = control_resamples(save_pred = TRUE))
  
  lasso_best = lasso_tune |> 
    select_by_one_std_err(metric = "roc_auc", -penalty)
  
  lasso_final = finalize_workflow(lasso_wf, lasso_best)
  lasso_final_fit = last_fit(lasso_final, split = theta_split)
  
  #### XGBoost ####
  print("Performing XGB Classification")
  xgb_spec = boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune(),                         ## step size
  ) |> 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  xgb_grid = grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), theta_train),
    learn_rate(),
    size = 30
  )
  
  xgb_wf = workflow() |> 
    add_recipe(rose_rec) %>%
    add_model(xgb_spec)
  
  doParallel::registerDoParallel()
  set.seed(234)
  xgb_tune = tune_grid(xgb_wf,
                       resamples = folds,
                       grid = xgb_grid,
                       control = control_grid(save_pred = TRUE)
  )
  
  xgb_best = select_best(xgb_tune, "roc_auc")
  xgb_final = finalize_workflow(xgb_wf, xgb_best)
  xgb_final_fit = last_fit(xgb_final, split = theta_split)

  #### SVM ####
  print("Performing SVM Classification")
  svm_spec = svm_linear(cost = tune()) |> 
    set_mode("classification") |> 
    set_engine("kernlab")
  
  svm_wf = workflow() |> 
    add_recipe(rose_rec) |> 
    add_model(svm_spec)
  
  doParallel::registerDoParallel()
  set.seed(234)
  svm_tune = tune_grid(svm_wf,
                       resamples = folds,
                       control = control_grid(save_pred = TRUE))
  
  svm_best = select_best(svm_tune, "roc_auc")
  svm_final = finalize_workflow(svm_wf, svm_best)
  svm_final_fit = last_fit(svm_final, split = theta_split)
  
  #### Save models ####
  print("Exporting")
  trained_mods = list(logistic = log_final_fit,
                      nb = nb_final_fit,
                      lasso = lasso_final_fit,
                      xgb = xgb_final_fit,
                      svm = svm_final_fit)
  
  rio::export(trained_mods, here("output",paste0("appendix_k",i,"_validation.rds")))
  rm(trained_mods, stm_n, stm_test, theta_train, theta_test, theta_split,
     folds, gen_rec, rose_rec, log_spec, log_wf, log_fit, log_final_fit,
     nb_spec, nb_wf, nb_grid, nb_tune, nb_best, nb_final, nb_final_fit,
     lasso_spec, lasso_wf, lasso_tune, lasso_best, lasso_final, lasso_final_fit,
     xgb_spec, xgb_wf, xgb_grid, xgb_tune, xgb_best, xgb_final, xgb_final_fit,
     svm_spec, svm_wf, svm_tune, svm_best, svm_final, svm_final_fit)
  gc()
}
