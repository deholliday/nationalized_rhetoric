trained_mods = rio::import(here("output","trained_mods.rds"))

model_tab = map_dfr(trained_mods, \(x){
  metrics = x |> 
    collect_metrics() |> 
    pivot_wider(names_from = .metric, values_from = .estimate) |> 
    select(accuracy, roc_auc)
  predictions = x |> 
    collect_predictions()
  pred.tab = table(predictions$.pred_class, predictions$fed_level)
  data.frame(
    nat_cor = pred.tab[1,1],
    nat_inc = pred.tab[2,1],
    state_cor = pred.tab[2,2],
    state_inc = pred.tab[1,2]
  ) |> 
    bind_cols(metrics)
}) |> mutate(model = names(trained_mods),
             across(accuracy:roc_auc, round, digits = 3)) |>
  select(model, everything())

model_tab |> 
  mutate(model = case_when(
    model == "logistic" ~ "Logistic Regression (Nonpenalized)",
    model == "nb" ~ "Naive Bayes",
    model == "lasso" ~ "Penalized Logistic Regression (Lasso)",
    model == "xgb" ~ "Boosted Gradient Descent (XGBoost)",
    model == "svm" ~ "Support Vector Machine"
  )) |> 
  kable(#format = "latex",
        caption = "Classification Model Performance on Heldout Documents",
        col.names = c("Model","Correct","Incorrect","Correct","Incorrect","Accuracy","AUC"),
        booktabs = T) |> 
  kable_classic(full_width = F) |> 
  add_header_above(c(" " = 1,
                     "National Documents" = 2,
                     "State Documents" = 2,
                     " " = 2)) |> 
  save_kable(file = here("tables","table_1.png"))