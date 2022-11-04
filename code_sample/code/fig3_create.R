log_ads_fit = rio::import(here("output","ads_mods.rds"))$logistic

# Confusion Matrix
p1_ads = log_ads_fit |> 
  collect_predictions() |>
  group_by(fed_level, .pred_class) |> 
  summarise(n = n())  |> 
  mutate(p = n/sum(n)) |>
  ungroup() |> 
  mutate(text_col = c(1,0,0,1)) |> 
  ggplot(aes(x = fed_level, y = .pred_class, fill = p)) + 
  geom_tile() +
  geom_text(aes(label = paste0(n," (",round(p*100,2),"%)"),
                color = as.factor(text_col)), size = 3) +
  scale_fill_gradient(low = "gray", high = "gray0") + 
  scale_color_manual(values = c("grey0", "white")) +
  scale_x_discrete(labels = c("National", "State")) +
  scale_y_discrete(labels = c("National", "State")) +
  theme_minimal() + theme(legend.position = "none") +
  xlab("Actual") + ylab("Prediction") + 
  labs(#title = "Figure 4: Ads Predictions",
    subtitle = "Confusion Matrix")

# Predicted Probabilities Distribution
p2_ads = log_ads_fit |> 
  collect_predictions() |> 
  filter(fed_level == "state") |> 
  ggplot(aes(x = .pred_national)) +
  geom_histogram() +
  geom_vline(xintercept = .5, color = "red", lty = 2) +
  theme_minimal() +
  ylab("Count") + xlab("P(National)") +
  labs(subtitle = "Gubernatorial ads P(National) \ndistribution")

# Patchwork together
fig3_ads = (p1_ads + p2_ads  + plot_layout(ncol = 2))
ggsave("fig3_ads.pdf", plot = fig3_ads,
       path = here("figures"),
       height = 3, width = 6, units = "in", dpi = 600)