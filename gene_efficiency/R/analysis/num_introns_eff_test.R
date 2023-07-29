# A one-way anova test will be performed to assess if there is a
# significant difference between the number of introns an 'efficient'
# gene has compared to its counterparts.

efficiency_intron_df <- data.frame(Efficiency = mSta.mCop$Efficiency,
                                   NumberIntrons = as.numeric(mSta.mCop$NumberIntrons))
efficiency_intron_df <- filter(efficiency_intron_df,
                               Efficiency != "Very Efficient")

anova_model <-  lm(NumberIntrons ~ Efficiency,
                   data=efficiency_intron_df)

anova(anova_model)
summary(anova_model)


# Check if residuals are normally distributed to assess whether
# assumptions of the model are met.
# hist(residuals(anova_model), col="darkgray")
# The residuals are not normally distributed therefore a kruskal-wallis
# test must be performed instead.

kruskal_result <- kruskal.test(NumberIntrons ~ Efficiency,
                               data = efficiency_intron_df)
# Kruska-Wallis test outputs a p-value of 1.051e-11 therefore there is a
# significant difference between the number of introns present in an
# efficient, standard and inefficient gene.

posthoc_result <- pairwise.wilcox.test(efficiency_intron_df$NumberIntrons,
                                       efficiency_intron_df$Efficiency,
                                       p.adjust.method = "bonferroni")

posthoc_result

efficiency_medians <- efficiency_intron_df %>%
  group_by(Efficiency) %>%
  summarise(median_NumberIntrons = median(NumberIntrons))

efficiency_medians
