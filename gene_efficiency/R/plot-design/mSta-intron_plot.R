proteins <- transform(proteins, NumberIntrons = as.factor(NumberIntrons))

mSta.intron <- ggplot(data = proteins, aes(x = NumberIntrons,
                                           y = log10(mRNA.stabilities),
                                           fill = "firebrick")) +
  geom_boxplot() +
  theme_tufte() +
  theme(panel.background = element_rect(fill = "#BFD5E3")) +
  theme(legend.position="none") +
  xlab("Number of Introns") +
  ylab("(log10) mRNA Half-life (mins)") +
  scale_x_discrete(labels = c("1" = "0", "2" = "1", "3" = "2",
                              "4" = "3", "5" = "4", "6" = "5",
                              "7" = "6", "8" = "7", "9" = "8",
                              "10" = "9", "11" = "10", "12" = "11",
                              "13" = "12", "14" = "13", "15" = "14",
                              "16" = "15"))

mSta.intron

ggsave("figures/mSta-intron_fig.jpeg", mSta.intron, width = 6, height = 4,
       dpi = 300)
