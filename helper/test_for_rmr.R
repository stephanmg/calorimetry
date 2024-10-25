library(ggplot2)

df <- read.csv2("test_for_rmr.csv")
df$Animal <- as.factor(df$Animal)
df$Component <- as.factor(df$Component)

p <- ggplot(df, aes(x=Animal, y=HP, color=Component)) + geom_boxplot() + geom_point(position=position_jitter(0.1))
ggsave("test_for_rmr.png")
