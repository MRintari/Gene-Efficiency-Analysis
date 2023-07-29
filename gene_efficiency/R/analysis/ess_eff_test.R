# Assessing if being an essential gene has an effect on whether a gene
# is efficient or not.

mSta.mCop %>% group_by(essential) %>%
  summarise(count = count(Efficiency))
# From the 70 efficient or very efficient genes, 11 are essential
# (15.7%). From the 4405 standard genes, 31.7% are essential.

# Make efficient genes dataframe, incorporate Very Efficient gene in dataset
# for analysis.
efficient_genes <- filter(mSta.mCop, Efficiency == "Efficient" |
                            Efficiency == "Very Efficient")

ggplot(data = efficient_genes, aes(x = essential, fill = Efficiency)) +
  geom_bar()


standard_genes <- filter(mSta.mCop, Efficiency == "Standard")

ggplot(data = standard_genes, aes(x = essential, fill = "red")) +
  geom_bar() +
  theme(legend.position = "none")

inefficient_genes <- filter(mSta.mCop, Efficiency == "Inefficient")

ggplot(data = inefficient_genes, aes(x = essential, fill = "red")) +
  geom_bar() +
  theme(legend.position = "none")

# To test if there is a significant difference in these values, a
# chi-square test of independence could be performed due to the number
# of normal genes. However, due to the small sample size of efficient
# genes, a Fisher's exact test will be performed instead.

ess_eff_df <- data.frame("Efficient" = c(11, 59), "Standard" = c(1397, 3008),
                      "Inefficient" = c(48, 132),
                      row.names = c("Essential", "Non-Essential"),
                      stringsAsFactors = FALSE)
ess_eff_df

ess_eff_fish <- fisher.test(ess_eff)
ess_eff_fish
# This Fisher's test p-value = 0.8864, therefore there is no significant
# relationship between an efficient gene transcript and whether it is
# essential or not.

# Plot of efficient genes compared to whether or not they are essential.
# Includes Chi-Square test p-value (p=0.81).

x <- c()
for (row in rownames(ess_eff)) {
  for (col in colnames(ess_eff)) {
    x <- rbind(x, matrix(rep(c(row, col), ess_eff[row, col]),
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
