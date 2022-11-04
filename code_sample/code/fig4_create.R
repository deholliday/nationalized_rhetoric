log_twitter_fit = rio::import(here("output","twitter_mods.rds"))$logistic

# Confusion Matrix
p1_twitter = log_twitter_fit |> 
  collect_predictions() |>
  group_by(fed_level, .pred_class) |> 
  summarise(n = n())  |> 
  mutate(p = n/sum(n)) |>
  ungroup() |> 
  mutate(text_col = c(1,1,0,1)) |> 
  ggplot(aes(x = fed_level, y = .pred_class, fill = p)) + 
  geom_tile() +
  geom_text(aes(label = paste0(n,"\n(",round(p*100,2),"%)"),
                color = as.factor(text_col)), size = 3) +
  scale_fill_gradient(low = "gray", high = "gray0") + 
  scale_color_manual(values = c("grey0", "white")) +
  scale_x_discrete(labels = c("National \n(Congress)", "State")) +
  scale_y_discrete(labels = c("National", "State")) +
  theme_minimal() + theme(legend.position = "none") +
  xlab("Actual") + ylab("Prediction") + 
  labs(#title = "Figure 5: Twitter Predictions",
    subtitle = "Confusion Matrix")

# Predicted Probabilities Distribution
p2_twitter = log_twitter_fit |> 
  collect_predictions() |> 
  filter(fed_level == "state") |> 
  ggplot(aes(x = .pred_national)) +
  geom_histogram() +
  geom_vline(xintercept = .5, color = "red", lty = 2) +
  theme_minimal() +
  ylab("Count") + xlab("P(National)") +
  labs(subtitle = "Gubernatorial Twitter P(National) \ndistribution")

# Patchwork together
fig4_twitter = (p1_twitter + p2_twitter  + plot_layout(ncol = 2))
ggsave("fig4_twitter.pdf", plot = fig4_twitter,
       path = here("figures"),
       height = 3, width = 6, units = "in", dpi = 600)
