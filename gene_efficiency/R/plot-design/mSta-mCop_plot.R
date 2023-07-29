# Plotting mRNA stability against mRNA copies allows for an assessment of
# transcription efficiency. Genes that produce many mRNA copies but have short
# half-lives would be consuming a large amount of energy.

mSta.mCop.plot <- ggplot(data = mSta.mCop,
                         aes(x = log10(mRNA_copies_per_cell),
                             y = log10(mRNA.stabilities),
                             colour = Efficiency, text = paste("Gene:", gene))) +
  geom_point(alpha = 0.65) +
  geom_smooth(se = FALSE, colour = 'yellow') +
  scale_color_hue(l=35, c=100) +
  xlab("(log10) mRNA copies per cell") +
  ylab("(log10) mRNA half-life (mins)") +
  #For reference during analysis visualisation
  # geom_hline(yintercept = log10(mSta_lower), colour = 'purple') +
  # geom_hline(yintercept = log10(mSta_upper), colour = 'blue') +
  # geom_vline(xintercept = log10(mCop_lower), colour = 'green') +
  # geom_vline(xintercept = log10(mCop_upper), colour = 'red') +
  theme_tufte() +
  scale_colour_discrete(breaks = c("Very Efficient", "Efficient",
                                   "Standard", "Inefficient"), l=35, c=100) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  theme(axis.line = element_line(size = 0.5, colour = "black")) +
  theme(legend.title=element_text(size=10))

ggsave("figures/mSta-mCop_fig.jpeg", mSta.mCop.plot, width = 6, height = 4,
       dpi = 300)
