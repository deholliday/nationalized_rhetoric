k_result = rio::import(here("data","stm_k_result.rds"))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Figure A1.1 Model Diagnostics by Number of Topics") +
  theme_bw() -> fig_a1_1_kdiagnostic

ggsave("fig_a1_1_kdiagnostic.pdf",  fig_a1_1_kdiagnostic,
       path = here("figures"),
       height = 5, width = 6, units = "in", dpi = 600)
