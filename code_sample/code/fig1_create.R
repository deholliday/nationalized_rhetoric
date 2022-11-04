cspan_test = rio::import(here("data","cspan_test.rds"))
log_cspan_fit = rio::import(here("output","cspan_mods.rds"))$logistic

# Confusion Matrix
p1_cspan = log_cspan_fit |> 
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
  labs(#title = "Figure 2: C-SPAN Debate Predictions",
    subtitle = "Confusion matrix")

# Predicted Probabilities Over Time
p1.1_cspan = log_cspan_fit |> 
  collect_predictions() |> 
  mutate(year_pooled = cspan_test$year_pooled) |>
  group_by(year_pooled, fed_level) |> 
  summarise(mean = mean(.pred_national)) |> 
  ggplot(aes(x = year_pooled, y = mean, group = fed_level, color = fed_level)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = .5, color = "red") +
  scale_color_manual(values = c("gray0", "gray"),
                     name = "Class",
                     labels = c("National", "State")) +
  theme_bw() +
  ylab("P(National)") +
  theme(#legend.position = "none",
    axis.title.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  labs(subtitle = "P(National) over time")

# Patchwork together
fig1_cspan = (p1_cspan + p1.1_cspan +  plot_layout(ncol = 2))
ggsave("fig1_cspan.pdf", plot = fig1_cspan,
       path = here("figures"),
       height = 3, width = 6, units = "in", dpi = 600)
