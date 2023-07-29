# Unused script, can be viewed as an example of how machine learning may be
# incorporated into this report. Given a lack of a word limit, this could be
# used to further explore the dataset.


library(tidyverse)
library(caret)
library(GGally)
library(ggbiplot)
library(plotly)

summary(gene)

gene %>%
  group_by(NumberIntrons) %>%
  summarise(count = n())

# Make data set only containing protein coding regions
proteins <- gene %>% subset(protein_coding == 1)
proteins <- proteins %>% select(-ncRNA, -protein_coding)


# Add gene length column
proteins$Gene_length <- (proteins$end - proteins$start) + 1

proteins$location <- NA
proteins[which(proteins$Golgi == 1),]$location <-  "Golgi"
proteins[which(proteins$Mitochondrion == 1),]$location <- "Mitochondrion"
proteins[which(proteins$Nucleus == 1),]$location <- "Nucleus"

yeast <-
  proteins %>%
  select(-Golgi, -Mitochondrion, -Nuclear_dots, -Nuclear_envelope, -Nucleolus,
         -Nucleus, -Vacuole, -start, -end, -solid.media.KO.fitness, -location,
         -GeneticDiversity) %>%
  na.omit()

pca <- yeast %>%
  select(-gene, -NumberIntrons, -essential, -chromosome) %>%
  prcomp(scale. = TRUE)

# Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric
# NA values have been removed and all values are numeric.

str(yeast)
# Rel_telomere appears to be stored as a factor variable instead of a numeric.
yeast$Rel_telomere <- as.numeric(yeast$Rel_telomere)

# Make number introns factor due to it not being a continuous quantitative
# variable
yeast$NumberIntrons <- as.factor(yeast$NumberIntrons)

# Perform PCA analysis
pca <- yeast %>%
  select(-gene, -NumberIntrons, -essential, -chromosome) %>%
  prcomp(scale. = TRUE)

summary(pca)

pca_labelled <- data.frame(pca$x, gene = yeast$gene,
                           NumberIntrons = yeast$NumberIntrons,
                           essential = yeast$essential,
                           chromosome = yeast$chromosome)

pca_labelled %>%
  select(-gene) %>%
  filter(complete.cases(.)) %>%
  ggpairs(aes(color = NumberIntrons),
          upper = list(continuous = wrap("cor", size = 1))) +
  theme(text = element_text(size = 6))

ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = pca_labelled$NumberIntrons)
# Make points transparent to see direction of variance distribution

plot_ly(x = pca_labelled$PC1, y = pca_labelled$PC2, z = pca_labelled$PC3,
        type = "scatter3d", mode = "markers",
        color = pca_labelled$NumberIntrons) %>%
  layout(scene = list(xaxis = list(title = "PC1 (36.1% explained var."),
                      yaxis = list(title = "PC2 (20.6% explained var."),
                      zaxis = list(title = "PC3 (11.9% explained var.")))

# Use different unsupervised learning algorithms
# Maybe make a venn diagram when looking at data regarding their locations?
