# Reproducing the same plot as first produced, comparing mRNA stability
# to the number of mRNA copies present per cell, using a transformed
# version of the protein copies per cell variable as a 3rd colour
# variable.

mSta.mCop <- mSta.mCop %>%
  mutate(log_protein_copies = log10(protein_copies_per_cell)) %>%
  mutate(log_protein_copies = round_any(log_protein_copies, 0.5))
mSta.mCop <-
  transform(mSta.mCop,log_protein_copies =
              as.factor(log_protein_copies))
# The protein copies per cell were logged to reduce the distribution and
# normalise the scale. A colour gradient would be needed to visually
# assess the increase in number of protein copies per cell, therefore
# the number of protein copies per cell needed to be discrete. To make
# this happen, the logged values were rounded to the nearest .5, where
# they were then transformed into a factor variable.

mSta.mCop.pCop.plot <- ggplot(data = mSta.mCop,
                              aes(x = log10(mRNA_copies_per_cell),
                                  y = log10(mRNA.stabilities),
                                  colour = log_protein_copies)) +
  geom_point() +
  geom_smooth(se = FALSE, colour = "yellow") +
  #scale_color_hue(l=35, c=100) +
  xlab("(log10) mRNA copies per cell") +
  ylab("(log10) mRNA half-life (mins)") +
  theme_clean() +
  scale_colour_brewer(palette = "PuRd", breaks = c("6", "5.5", "5",
                                                   "4.5", "4", "3.5",
                                                   "3", "2.5", "2"),
                      name = "Protein Copies (log10)") +
  theme(panel.background = element_rect(fill = "#BFD5E3"),
        legend.background = element_rect(fill = "#BFD5E3"),
        legend.key = element_rect(fill = "black", color = NA),
        panel.border = element_blank(), axis.line = element_blank())

ggsave("figures/mSta-mCop-pCop_fig.jpeg", mSta.mCop.pCop.plot, width = 6,
       height = 4, dpi = 300)
