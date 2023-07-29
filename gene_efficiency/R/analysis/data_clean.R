# As this is an investigation in protein synthesis, we only require genes that
# produce proteins. Therefore, the first step is the removal of non-coding
# genes.
proteins <- gene %>% subset(protein_coding == 1)

# We no longer need the ncRNA column as all values should be 0. The same
# applies for the protein_coding column.
proteins <- proteins %>% select(-ncRNA, -protein_coding)

# We can replace the start and end columns with a single column that shows the
# length of the gene.
proteins <- mutate(proteins, gene_length = (proteins$end -
                                              proteins$start) + 1) %>%
  select(-start, -end)

# As certain genes may have missing values for their mRNA stability but not
# their protein copies or protein stability, we don't want to omit all of the
# data for this gene when assessing the relationships between these properties.
# Therefore, to still find outlier values, subset dataframes must be created
# instead.

# Creating !is.na() subset dataframes
no_mSta_NAs <- subset(proteins, !is.na(mRNA.stabilities))
no_mCop_NAs <- subset(proteins, !is.na(mRNA_copies_per_cell))
no_pSta_NAs <- subset(proteins, !is.na(ProteinHalfLife))
no_pCop_NAs <- subset(proteins, !is.na(protein_copies_per_cell))

# NumberIntrons column must be converted to
