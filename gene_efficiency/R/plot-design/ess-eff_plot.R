# Plot of comparing the efficiency of genes to whether or not they are
# essential.
# Includes Chi-Square test p-value (p=).

x <- c()
for (row in rownames(ess_eff_count)) {
  for (col in colnames(ess_eff_count)) {
    x <- rbind(x, matrix(rep(c(row, col), ess_eff_count[row, col]),
                         ncol = 2, byrow = TRUE))
  }
}
ess_eff_df <- as.data.frame(x)
colnames(ess_eff_df) <- c("Is_Essential", "Efficiency")
ess_eff_df

ess_eff.plot <- ggbarstats(data = ess_eff_df, x = Is_Essential,
                           y = Efficiency) +
  labs(caption = NULL) +
  theme_tufte() +
  theme(legend.title = element_blank())

ess_eff.plot

ggsave("figures/ess-eff_fig.jpeg", ess_eff.plot, width = 6, height = 4,
       dpi = 300)
