# libraries
library(doBy)
library(dplyr)
library(ggpubr)
library(patchwork)

# fake data (assuming the entries are one point in an arbitrary interval and
# that data has been interpolated already to a regular time grid with width e.g. 5 minutes)
#N = 50
#M = 1
# TODO use seed for synthetic data
#O2 = replicate(100, 0)
#CO2 = replicate(50, 1)
#CoV1 = replicate(50, 0.3)
#CoV2 = replicate(50, 0.2)
#HP = c(replicate(25, 100), replicate(25, 0))
#df <- data.frame(O2, CO2, CoV1, CoV2, HP)
#colnames(df) <- c("O2", "CO2", "CoV1", "CoV2", "HP")
#O2   = runif(n=N, min=0, max=1)
#CO2  = runif(n=N, min=0, max=1)
#CoV1 = runif(n=N, min=0, max=0.5)
#CoV2 = runif(n=N, min=0, max=0.5)
#HP   = runif(n=N, min=0, max=100)
#df <- data.frame(O2, CO2, CoV1, CoV2, HP)
#colnames(df) <- c("O2", "CO2", "CoV1", "CoV2", "HP")


#########################
# real data
#########################
#N = 800 # total intervals
#M = 50 # number of data points in an interval (needed for time trace reconstruction over 24h day)
#SLIDING_WINDOW_OF_PREPROCESSING = 10
#data <- read.csv2("df_for_cov_analysis.csv", sep=";")

do_extract <- function(df, component="O2", percentage=5, N) {
   # order df by component O2
   df_ordered = df[order(df[[component]]),]

   # print 
   indices = which.minn(df_ordered[[component]], n=N*percentage/100)

   sub_df = df_ordered[indices,]
   print(sub_df)

   psum <- function(...,na.rm=FALSE) { 
       rowSums(do.call(cbind,list(...)),na.rm=na.rm) } 

   dat <- transform(sub_df, sum = psum(CoV1, CoV2))
   print(dat)

   # today do not return only the best but the 10% lowest in the interval
   index = which.minn(dat$sum, n=1) # TODO: return n=3 best values, then do binned scatter plot (y axis)
   # plot data of heat production by using index
   print("RMR in this measurement (interval):")
   # TODO: HP needs to be calculated with O2 and CO2 and all indices... (index from above!)
   print(dat[index,]$HP)
   dat[index,]$HP
   # HP means really heat production, either sorted by O2 or CO2 production or not
}

create_df <- function(df, component, M, N, percentage) {
   hp = c()
   index = c()


   # TODO: Time N/M -> N number of data entries, equally spaced typically 10 minutes, M how many intervals...
   for (i in 0:floor(N/M)) { # sub interval, get minimum of every M elements...
      # extract only from some interval (could also extract from whole df!)
      hp_val <- do_extract(df[seq(i*M, i*M+M),], component, percentage, N)
      if (length(hp_val) != 0) { # means couldn't extract a minimum in the function do_extract: question figure out why... TODO
         hp <- append(hp, hp_val)
         index <- append(index, i)
      }
   }
   df_plot <- data.frame(hp, index)
   colnames(df_plot) <- c("HP", "Time")
   INTERVAL_LENGTH = 10
   #df_plot$Time <- df_plot$Time * floor(N/M) # which averaging (time points in dat taken every 5 minutes?)
   df_plot$Time <- df_plot$Time * INTERVAL_LENGTH  * (M / INTERVAL_LENGTH)
   df_plot$HP <- df_plot$HP / 24 / (60/INTERVAL_LENGTH) / INTERVAL_LENGTH
   #df_plot$HP <- df_plot$HP / 24 / 6 # heat production divided by 24 hours (might be scaled wrongly here) 
   # TODO: need to scale HP to hour first, as it may be only one single value extracted, is has to be accounted for the time interval length
   # TODO: Interval llength needs to be known 5 or 10 minutes to scale HP...
   df_plot
}

filename = "test_O2.pdf"
percentage=5

extract_rmr2 <- function(data, M, PERCENTAGE) {
   N <- nrow(data)
   M = 10
   df <- data
   df_plot_O2 <- create_df(df, "O2", M, N, PERCENTAGE)
   df_plot_CO2 <- create_df(df, "CO2", M, N, PERCENTAGE)
   df_foo <- data.frame(df$HP, seq(1, N))
   colnames(df_foo) <- c("HP", "Time")
   df_foo$HP <- df_foo$HP / 24 # TODO/FIXME: dubious constant?! over day? but maybe okay....
   df_foo$Time <- df_foo$Time
   df_plot_total <- rbind(df_plot_O2, df_plot_CO2)
   df_plot_total$Component <- c(rep("O2", nrow(df_plot_O2)), rep("CO2", nrow(df_plot_CO2)))
   return (list("df_plot_total"=df_plot_total, "df_foo"=df_foo))
}

extract_rmr <- function(input_filename, M, PERCENTAGE, SLIDING_WINDOW_OF_PREPROCESSING=10, SEP=";") {
   data <- read.csv2(input_filename, sep=SEP)
   N <- nrow(data)
   df <- data
   df_plot_O2 <- create_df(df, "O2", M, N, PERCENTAGE)
   df_plot_CO2 <- create_df(df, "CO2", M, N, PERCENTAGE)
   df_foo <- data.frame(df$HP, seq(1, N))
   colnames(df_foo) <- c("HP", "Time")
   df_foo$HP <- df_foo$HP / 24 # TODO/FIXME: dubious constant?! over day? but maybe okay....
   df_foo$Time <- df_foo$Time
   df_plot_total <- rbind(df_plot_O2, df_plot_CO2)
   df_plot_total$Component <- c(rep("O2", nrow(df_plot_O2)), rep("CO2", nrow(df_plot_CO2)))
   return (list("df_plot_total"=df_plot_total, "df_foo"=df_foo))
}

#df_plot_O2 <- create_df(df, "O2")
#df_plot_CO2 <- create_df(df, "CO2")

#df_foo <- data.frame(df$HP, seq(1, N))
#colnames(df_foo) <- c("HP", "Time")
#df_foo$HP <- df_foo$HP / 24
#df_foo$Time <- df_foo$Time

#df_plot_total <- rbind(df_plot_O2, df_plot_CO2)
#df_plot_total$Component <- c(rep("O2", nrow(df_plot_O2)), rep("CO2", nrow(df_plot_CO2)))
# need to do this by group!
##p <- ggscatter(df_plot, x="Time", y="HP")

## Not required!
#M = 50
#PERCENTAGE=5
#dd <- extract_rmr("df_for_cov_analysis.csv", M, PERCENTAGE)
#df_plot_total <- dd$df_plot_total
#df_foo <- dd$df_foo
#p <- ggline(df_plot_total, x="Time", y="HP", color="Component")
#p <- p + rotate_x_text(90)
#p <- ggpar(p, xlab="Time [h]", title="Sliding Window = 10, # Meas = 800, # Meas / Int = 50, Length of Int = 4 h", subtitle="Animal 2265 (TSE file: 20200508_SD_Ucpdd_K1.csv)", ylab="RMR [kcal/day]", legend.title="Sorted by component")

#p2 <- ggline(df_foo, x="Time", y="HP") 
#p2 <- ggpar(p2, ylab="TEE [kcal/day]")
#p2 <- p2 + rotate_x_text(90)

#p <- p | p2
#p %>% ggexport(filename=filename)
