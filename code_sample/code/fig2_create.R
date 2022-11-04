cspan_test = rio::import(here("data","cspan_test.rds"))

cspan_test[395,] |> 
  select(starts_with("V")) |> 
  pivot_longer(V1:V40, names_to = "topic", values_to = "prop") |> 
  mutate(coef = log_cspan_fit |> 
           extract_fit_engine() |> 
           broom::tidy() |> 
           filter(term != "(Intercept)") |> 
           pull(estimate),
         p.val = log_cspan_fit |> 
           extract_fit_engine() |> 
           broom::tidy() |> 
           filter(term != "(Intercept)") |> 
           pull(p.value),
         sig = ifelse(p.val < .05, 1, 0),
         cat = case_when(
           sig == 0 ~ "None",
           coef > 0 ~ "State",
           coef < 0 ~ "National"
         ),
         topic = str_remove_all(topic, "V")) |> 
  ggplot(aes(area = prop, fill = coef,
             label = topic, subgroup = cat)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white") +
  geom_treemap_subgroup_text(place = "bottomright", grow = T, alpha = 0.5, 
                             colour = "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "black", alpha = .3, place = "topleft", reflow = T) +
  scale_fill_gradient2(name = "Log-Odds Coefficient") +
  theme(legend.position = "bottom") -> fig2_treemap

ggsave("fig2_treepmap.pdf", plot = fig2_treemap,
       path = here("figures"),
       height = 4, width = 6, units = "in", dpi = 600)
