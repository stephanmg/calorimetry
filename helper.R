library(dplyr)
library(ggplot2)

################################################################################
# partition
################################################################################
# mydf, data
partition <- function(mydf) {
   df <- mydf
   data <- df %>% group_split(Group)
   df_new <- data.frame()
   for (i in data) {
        if (nrow(df_new) == 0) {
            df_new = data.frame(c(i$Values))
        } else {
            df_new <- cbind(df_new, c(i$Values))
        }
   }
   colnames(df_new) <- unique(df$Group)
   df_new
}

################################################################################
# cv
################################################################################
# mydf, data
# window, size
cv <- function(mydf, window = 2) {
   df <- mydf
   df_new <- data.frame()
   for (i in 1:ncol(df)) {
      values <- df[, i]
      covs <- c()
      for (j in seq(from = 1, to = length(values), by = 1)) {
         m <- mean(values[seq(from = j, to = j + window - 1, by = 1)])
         s <- sd(values[seq(from = j, to = j + window - 1, by = 1)])
         covs <- append(covs, s / m)
         # find the m index which is lowest in CoV and energy expenditure
      }
      if (nrow(df_new) == 0) {
         df_new <- data.frame(covs)
      } else {
         df_new <- cbind(df_new, covs)
      }
   }
   colnames(df_new) <- names(df)
   df_new
}

################################################################################
# reformat
################################################################################
# df_new, data
reformat <- function(df_new) {
   df_final <- data.frame(HP = c(), Group = c())
   for (i in 1:ncol(df_new)) {
      df_tmp <- data.frame(HP=df_new[, i], Group=rep(colnames(df_new)[i], length(df_new[, i]))) #nolint
      df_final <- rbind(df_final, df_tmp)
   }
   df_final
}
