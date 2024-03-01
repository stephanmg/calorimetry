# libraries
library(doBy)
library(dplyr)
library(ggpubr)
library(patchwork)

################################################################################
# do_extract
################################################################################
# N specifies the number of total intervals of time increase of e.g. 15 minutes
# M specifies the number of intervals one wishes to find a minimum energy exp.
# percentage specifies how many of the minimum values should be considered
do_extract <- function(df, component = "O2", percentage = 5, N) {
   # order df by component O2
   df_ordered <- df[order(df[[component]]), ]

   # indices of minimum energy expenditure
   indices <- which.minn(df_ordered[[component]], n = N * percentage / 100)
   # extract a sub data frame from the indices
   sub_df <- df_ordered[indices, ]
   # print(sub_df)

   # partial rowsum
   psum <- function(..., na.rm = FALSE) {
       rowSums(do.call(cbind, list(...)), na.rm = na.rm)
   }

   # apply partial rowsum to CoV1 (O2) and CoV2 (CO2)
   dat <- transform(sub_df, sum = psum(CoV1, CoV2))
   # print(dat)

   # FIXME: return n > 1 best values, then create a binned scatter plot (y axis)
   index <- which.minn(dat$sum, n = 1)
   # plot data of minimum energy expenditure per interval with one index
   # print("RMR in this measurement (interval):")

   # FIXME: HP needs to be calculated with O2 and CO2 an all indices n > 1 see above
   # HP means heat production, either sorted by O2 or CO2
   # print(dat[index, ]$HP)
   dat[index, ]$HP
}

################################################################################
# create_df
################################################################################
# df, data frame
# component, either O2 or CO2
# M, sliding window size, typically much smaller than N
# N, total intervals
# percentage, how many of best (minimum energy expenditure values) to consider
create_df <- function(df, component, M, N, percentage, interval_length = 15) {
   hp <- c()
   index <- c()
   for (i in 0:floor(N / M)) { # sub interval, get minimum EE in M intervals
      hp_val <- do_extract(df[seq(i * M, i * M + M), ], component, percentage, N)
      # Minimum EE couldn't be extracted, but why? (End of intervals?)
      if (length(hp_val) != 0) {
         hp <- append(hp, hp_val)
         index <- append(index, i)
      }
   }
   df_plot <- data.frame(hp, index)
   colnames(df_plot) <- c("HP", "Time")
   df_plot$Time <- df_plot$Time * interval_length  * (M / interval_length)
   df_plot$HP <- df_plot$HP / 24 / (60 / interval_length) / interval_length
   df_plot
}

# default settings for test
filename <- "test_O2.pdf"
percentage <- 5

################################################################################
# extract_rmr
################################################################################
# data
# M
# PERCENTAGE
extract_rmr <- function(data, M, PERCENTAGE, interval_length = 15) {
   N <- nrow(data)
   # TODO: Make M a parameter... a user might want to specify the sliding window
   M <- 5
   # actual data
   df <- data
   df_plot_O2 <- create_df(df, "O2", M, N, PERCENTAGE, interval_length)
   df_plot_CO2 <- create_df(df, "CO2", M, N, PERCENTAGE, interval_length)
   df_foo <- data.frame(df$HP, seq(1, N))
   colnames(df_foo) <- c("HP", "Time")
   df_foo$HP <- df_foo$HP / 24 # Note normalize over day (24 hours)
   df_foo$Time <- df_foo$Time
   df_plot_total <- rbind(df_plot_O2, df_plot_CO2)
   df_plot_total$Component <- c(rep("O2", nrow(df_plot_O2)),
      rep("CO2", nrow(df_plot_CO2)))
   return(list("df_plot_total" = df_plot_total, "df_foo" = df_foo))
}
