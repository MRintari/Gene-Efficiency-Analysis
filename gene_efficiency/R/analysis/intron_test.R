# To statistically test for this, we first need to see if the datasets
# are normally distributed. This can be assumed due to the large sample
# size, however this will be tested for to ensure this is the case.

lack_introns <- subset(proteins, proteins$LackIntrons == TRUE)
have_introns <- subset(proteins, proteins$LackIntrons == FALSE)
shapiro.test(lack_introns$mRNA.stabilities)
# Appears to be significantly different from normal distribution,
# p < 2.2e-16.
shapiro.test(have_introns$mRNA.stabilities)
# p < 2.2e-16

# As the two groups of data are not normally distributed, a Wilcoxon
# test will be performed to test for a significant difference in mRNA
# half-life for genes that have introns and genes that do not.

mSta.intron.test <- wilcox.test(mRNA.stabilities ~ LackIntrons, data = proteins,
                                exact=FALSE)

lack_introns_Sta <- subset(lack_introns, !is.na(mRNA.stabilities))

have_introns_Sta <- subset(have_introns, !is.na(mRNA.stabilities))

shapiro.test(lack_introns$mRNA_copies_per_cell)
shapiro.test(have_introns$mRNA_copies_per_cell)
# Neither data group is normally distributed, with both having p-values
# < 2.2e-16

mCop.intron.test <- wilcox.test(mRNA_copies_per_cell ~ LackIntrons, data = proteins,
                                exact = FALSE)

lack_introns_Cop <- subset(lack_introns, !is.na(mRNA_copies_per_cell))

have_introns_Cop <- subset(have_introns, !is.na(mRNA_copies_per_cell))
