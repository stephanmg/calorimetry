library(readxl)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggExtra)

plot_locomotion <- function(file) {
	sheet <- "Behavior_List"

	df <- read_excel(file, sheet=sheet)
	df <- na.omit(df)
	animals = unique(df$Animal)
	print("animals:")
	print(animals)
	rects <-  data.frame(xmin=c(2,20.5,20.5), xmax=c(9.5, 29, 28), ymin=c(4, 4.2, 12), ymax=c(16, 9.5, 15.5), r=c("Running Wheel", "Food Hopper", "Water Bottle"))
	df <- df %>% group_by(Animal) %>% mutate(Durat_Sec_Norm = Durat_Sec/max(Durat_Sec))
	rows = floor(sqrt(length(unique(df$Animal))))
	cols = rows
	print("number of cols")
	print(cols)

	df <- df[rep(seq(nrow(df)), df$Durat_Sec),]
	p2 <- ggplot(df, aes(Y_cm, X_cm)) + geom_density_2d_filled(contour_var="ndensity") 
	p2 <- p2 + facet_wrap(~ Animal, ncol=cols)
	p2 <- p2 + xlab("Position Y [cm]")
	p2 <- p2 + ylab("Position X [cm]")
	p2 <- p2 + ggtitle(paste('Position (Y, X) probability heat map for ', length(unique(df$Animal)), " animals", sep=""))
	p2 <- p2 + geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), color="white", alpha=0.0)
	p2 <- p2 + geom_text(data=rects, inherit.aes=FALSE, aes(x=xmin+(xmax-xmin)/2, y=ymin+(ymax-ymin)/2, label=r), color="white", size=1)
	p2 <- p2 + scale_y_continuous(breaks=c(0, 5, 10, 15, 20)) + scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30))
	p2 <- p2 + guides(fill=guide_legend(title="Probability"))
	p2
}