# Comparison between mRNA copies and mRNA stability

# To assess which genes produce low copies of mRNA or have a low stability,
# outliers must be calculated. In this investigation, values that fall outside
# the interval formed by the 2.5 and 97.5 percentiles are considered outliers.

# Finding outliers for each piece of data
lower <- 0.025
upper <- 0.975

mSta_lower <- quantile(no_mSta_NAs$mRNA.stabilities, lower)
mSta_upper <- quantile(no_mSta_NAs$mRNA.stabilities, upper)
mCop_lower <- quantile(no_mCop_NAs$mRNA_copies_per_cell, lower)
mCop_upper <- quantile(no_mCop_NAs$mRNA_copies_per_cell, upper)

# mSta_LQ <- quantile(no_mSta_NAs$mRNA.stabilities, 0.25)
# mSta_UQ <- quantile(no_mSta_NAs$mRNA.stabilities, 0.75)
# mSta_IQR <- mSta_UQ - mSta_LQ
# mSta_lower <- mSta_LQ - (1.5*mSta_IQR)
# mSta_upper <- mSta_UQ + (1.5*mSta_IQR)
#
# mCop_LQ <- quantile(no_mCop_NAs$mRNA_copies_per_cell, 0.25)
# mCop_UQ <- quantile(no_mCop_NAs$mRNA_copies_per_cell, 0.75)
# mCop_IQR <- mCop_UQ - mCop_LQ
# mCop_lower <- mCop_LQ - (1.5*mCop_IQR)
# mCop_upper <- mCop_UQ + (1.5*mCop_IQR)


# Creating data set containing no NA values in either the mRNA stabilities
# column or the mRNA copies per cell column.
mSta.mCop <- no_mCop_NAs
mSta.mCop <- subset(mSta.mCop, !is.na(mRNA.stabilities))
mSta.mCop$Efficiency <- NA # Creating column to store efficiency score

# Storing the efficiency scores of the genes

# Genes that produce few transcripts and have long mRNA half-lives are deemed
# very efficient.
mSta.mCop[which(mSta.mCop$mRNA.stabilities >=
                  mSta_upper & mSta.mCop$mRNA_copies_per_cell <=
                  mCop_lower),]$Efficiency <- "Very Efficient"

# Genes that have long mRNA half-lives that produce a standard amount of
# transcripts are deemed efficient.
mSta.mCop[which(mSta.mCop$mRNA.stabilities >= mSta_upper &
                  (mSta.mCop$mRNA_copies_per_cell < mCop_upper &
                     mSta.mCop$mRNA_copies_per_cell >
                     mCop_lower)),]$Efficiency <- "Efficient"

# Genes that produce few transcripts and have normal mRNA half-lives are deemed
# efficient.
mSta.mCop[which(mSta.mCop$mRNA_copies_per_cell <= mCop_lower &
                  (mSta.mCop$mRNA.stabilities < mSta_upper &
                     mSta.mCop$mRNA.stabilities >
                     mSta_lower)),]$Efficiency <- "Efficient"

# Genes that find themselves producing a standard amount of mRNA transcripts
# with normal mRNA half-lives are deemed standard.
mSta.mCop[which((mSta.mCop$mRNA.stabilities > mSta_lower &
                   mSta.mCop$mRNA.stabilities < mSta_upper) &
                  (mSta.mCop$mRNA_copies_per_cell > mCop_lower &
                     mSta.mCop$mRNA_copies_per_cell <
                     mCop_upper)),]$Efficiency <- "Standard"

# Genes that produce a large amount of mRNA transcripts but also produce
# transcripts with long mRNA half-lives are deemed standard.
mSta.mCop[which(mSta.mCop$mRNA_copies_per_cell >= mCop_upper &
                  mSta.mCop$mRNA.stabilities >=
                  mSta_upper),]$Efficiency <- "Standard"

# Genes that produce transcripts with short mRNA half-lives but produce a small
# number of these transcripts are deemed standard.
mSta.mCop[which(mSta.mCop$mRNA.stabilities <= mSta_lower &
                  mSta.mCop$mRNA_copies_per_cell <=
                  mCop_lower),]$Efficiency <- "Standard"

# Genes that produce a standard amount of mRNA transcripts with short mRNA
# half-lives are deemed inefficient.
mSta.mCop[which((mSta.mCop$mRNA_copies_per_cell > mCop_lower &
                  mSta.mCop$mRNA_copies_per_cell < mCop_upper) &
                  mSta.mCop$mRNA.stabilities <=
                  mSta_lower),]$Efficiency <- "Inefficient"

# Genes that produce a large amount of mRNA transcripts with standard mRNA
# half-lives are deemed inefficient.
mSta.mCop[which(mSta.mCop$mRNA_copies_per_cell >= mCop_upper &
                  (mSta.mCop$mRNA.stabilities > mSta_lower &
                     mSta.mCop$mRNA.stabilities <
                     mSta_upper)),]$Efficiency <- "Inefficient"
#
# Genes with short mRNA half-lives AND produce many transcripts are deemed very
# inefficient.
# mSta.mCop[which(mSta.mCop$mRNA.stabilities <= mSta_lower &
#                  mSta.mCop$mRNA_copies_per_cell >=
#                  mCop_upper),]$Efficiency <- "Very Inefficient"

# This code has been commented out as none of the genes met the criteria for
# this condition, producing the following error:
# Error in `$<-.data.frame`(`*tmp*`, Efficiency, value = "Very Inefficient") :
# replacement has 1 row, data has 0

