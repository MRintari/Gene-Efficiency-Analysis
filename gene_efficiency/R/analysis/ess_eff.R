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
# chi-square test of independence can be performed.

ess_eff_count <- data.frame("Efficient" = c(11, 59), "Standard" = c(1397, 3008),
                      "Inefficient" = c(48, 132),
                      row.names = c("Essential", "Non-Essential"),
                      stringsAsFactors = FALSE)
ess_eff_count

ess_eff_chi <- chisq.test(ess_eff_count)
ess_eff_chi
# This test produces a p-value of 0.007, therefore there is a significant
# association between the efficiency of a gene transcript and whether it is
# essential or not.

