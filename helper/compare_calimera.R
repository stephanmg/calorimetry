library(ggplot2)

df = read.csv2("df_for_comparison_with_calimera.csv")
df$HP = as.numeric(df$HP)
#df = read.csv2("df_for_cov_analysis.csv")

p <- ggplot(data=df, aes(x=Time, y = HP, group = Component, color=Component)) + 
  geom_line() + facet_wrap(~Animal) 
p <- p + scale_y_continuous(breaks = pretty(df$HP, n = 10))

ggsave(p, filename="comparison.png")
