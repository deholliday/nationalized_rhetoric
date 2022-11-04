## code_stm_classification.R

#### Import Data/Split ####
theta_split = rio::import(here("data","theta_split.rds"))
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
log_spec = logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")

log_wf = workflow() |> 
  add_recipe(rose_rec) |> 
  add_model(log_spec)

log_fit = fit(log_wf, data = theta_train)
log_final_fit = last_fit(log_fit, theta_split)

#### Naive Bayes ####
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

# Plot Results
#quick_conf(nb_final_fit)

#### Lasso ####
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

# Plot Results
#quick_conf(lasso_final_fit)

#### XGBoost ####
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

# Plot Results
#quick_conf(xgb_final_fit)

#### SVM ####
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

# Plot Results
#quick_conf(svm_final_fit) # Confusion Matrix
#w = t(out@xmatrix[[1]]) %*% out@coef[[1]] # Coefficients for Topics

#### Save models ####
trained_mods = list(logistic = log_final_fit,
                    nb = nb_final_fit,
                    lasso = lasso_final_fit,
                    xgb = xgb_final_fit,
                    svm = svm_final_fit)

rio::export(trained_mods, here("output","trained_mods.rds"))