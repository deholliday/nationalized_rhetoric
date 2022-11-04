cspan_mods = rio::import(here("output","cspan_mods.rds"))
ads_mods = rio::import(here("output","ads_mods.rds"))
twitter_mods = rio::import(here("output","twitter_mods.rds"))

tab_func = \(data, title){
  model_tab = map_dfr(data, \(x){
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
  }) |> mutate(model = names(data),
               nat_acc = round(nat_cor/(nat_cor+nat_inc), digits = 3),
               state_acc = round(state_cor/(state_cor+state_inc), digits = 3),
               across(accuracy:roc_auc, round, digits = 4)) |> 
    select(model, starts_with("nat"), starts_with("state"), everything(), -accuracy, -roc_auc)
  
  model_tab |> 
    mutate(model = case_when(
      model == "logistic" ~ "Logistic Regression (Nonpenalized)",
      model == "nb" ~ "Naive Bayes",
      model == "lasso" ~ "Penalized Logistic Regression (Lasso)",
      model == "xgb" ~ "Boosted Gradient Descent (XGBoost)",
      model == "svm" ~ "Support Vector Machine"
    )) |> 
  kable(caption = title,
        col.names = c("Model","Correct","Incorrect","Accuracy",
                      "Correct","Incorrect","Accuracy"),
        booktabs = T) |> 
    kable_classic(full_width = F) |> 
    add_header_above(c(" " = 1,
                     "National Documents" = 3,
                     "State Documents" = 3))
}

tab_func(cspan_mods, "Table A2.1: Classification Model Performance on C-SPAN Debates") |> save_kable(file = here("tables","tab_a2_1.png"))
tab_func(ads_mods, "Table A2.2: Classification Model Performance on TV Ads") |> save_kable(file = here("tables","tab_a2_2.png"))
tab_func(twitter_mods, "Table A2.3: Classification Model Performance on Twitter") |> save_kable(file = here("tables","tab_a2_3.png"))