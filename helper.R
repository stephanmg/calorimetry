library(dplyr)
library(ggplot2)

# TODO: get rid of mock data here for testing purposes
# mock data: replace with real data, Group=AnimalNo/Group, Values=TEE for heat production formula #1, Values2=TEE for heat production formula #2
# TEE comes from roll mean with averaging window = 3 or so, before we average data to half hours of 10 minutes ... (do we need all of this?)
df <- data.frame(Values=c(1,2,3,4,57,8,9,10), Group=c("A", "A", "A", "A", "B", "B", "B", "B"), Values2=c(1,2,3,4,5,6,7,8))

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
         df_new <- cbin# nolintd(df_new, covs)
      }
   }
   colnames(df_new) <- names(df)
   df_new
}

df_new <- partition(df)
df_new <- cv(df_new)
print(df_new)

reformat <- function(df_new) {
   df_final <- data.frame(HP = c(), Group = c())
   for (i in 1:ncol(df_new)) {
      df_tmp <- data.frame(HP=df_new[, i], Group=rep(colnames(df_new)[i], length(df_new[, i]))) #nolint
      df_final <- rbind(df_final, df_tmp)
   }
   df_final
}

df_final <- reformat(df_new)
print(df_final)