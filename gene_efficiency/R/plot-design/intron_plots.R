proteins$LackIntrons <- NA
proteins[which(proteins$NumberIntrons == 0),]$LackIntrons <- TRUE
proteins[which(proteins$NumberIntrons != 0),]$LackIntrons <- FALSE

proteins$LackIntrons <- factor(proteins$LackIntrons,
                               levels=c(TRUE, FALSE))

# Plotting distribution of mRNA stability and number of mRNA copies for
# genes that have introns compared to genes that do not.

sta_intron_plot <- ggplot(data = proteins,
                          aes(x=LackIntrons,
                              y=log10(mRNA.stabilities),
                              fill = LackIntrons)) +
  geom_violin() +
  theme_tufte() +
  theme(legend.position="none") +
  xlab("") +
  ylab("Half-life") +
  theme(axis.text.x=element_blank()) +
  scale_fill_brewer(palette="Dark2") +
  theme(panel.background = element_rect(fill = "#BFD5E3"))

cop_intron_plot <- ggplot(data = proteins,
                          aes(x=LackIntrons,
                              y=log10(mRNA_copies_per_cell),
                              fill = LackIntrons)) +
  geom_violin() +
  theme_tufte() +
  theme(legend.position="none") +
  xlab("Lacks Introns") +
  ylab("Copies") +
  scale_fill_brewer(palette="Dark2") +
  theme(panel.background = element_rect(fill = "#BFD5E3"))

intron_plots <- ggarrange(sta_intron_plot, cop_intron_plot, ncol = 1,
                          nrow = 2)

ggsave("figures/intron_fig.jpeg", intron_plots, width = 6, height = 4,
       dpi = 300)
