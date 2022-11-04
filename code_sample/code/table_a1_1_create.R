map_dfr(seq(10,100,10),\(i){
  k_mods = rio::import(here("output",paste0("appendix_k",i,"_validation.rds")))
  map_dfr(k_mods, \(x){
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
  }) |> mutate(model = names(k),
               across(accuracy:roc_auc, round, digits = 3),
               k = i) |>
    select(model, k, accuracy)
}) |> 
  pivot_wider(id_cols = k, names_from = model,
              values_from = accuracy) |> 
  kable(caption = "Classification Model Performance on Heldout Documents - Varying K",
        col.names = c("K","Logistic Regression (Nonpenalized)",
                      "Naive Bayes","Penalized Logistic Regresssion (Lasso)",
                      "Boosted Gradient Descent (XGBoost)",
                      "Support Vector Machine"),
        booktabs = T) |> 
  kable_classic(full_width = F) |> 
  save_kable(file = here("tables","tab_a1_1.png"))
