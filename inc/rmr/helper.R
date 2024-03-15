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
            df_new <- data.frame(c(i$Values))
        } else {
            print(df_new)
            print(c(i$Values))
            df_new <- cbind(df_new, c(i$Values))
        }
   }
   colnames(df_new) <- unique(df$Group)
   df_new
}

partition2 <- function(mydf) {
   df <- mydf
   data <- df %>% group_split(Group)
   df_new <- data.frame()
   m = max(sapply(data, nrow))
   for (i in data) {
      diff = m - length(i$Values) 
      if (nrow(df_new) == 0) {
         if (length(c(i$Values)) != m) {
           df_new <- data.frame(c(i$Values, rep("NA", diff)))
         } else {
           df_new <- data.frame(c(i$Values))
         }
      } else {
         if (length(c(i$Values)) != m) {
            df_new <- cbind(df_new, c(i$Values, rep("NA", diff)))
         } else {
            print("there")
            df_new <- cbind(df_new, c(i$Values))
         }
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
   for (i in seq_len(ncol(df))) {
      values <- as.numeric(df[, i])
      covs <- c()
      for (j in seq(from = 1, to = length(values), by = 1)) {
         m <- mean(values[seq(from = j, to = j + window - 1, by = 1)], na.rm = TRUE)
         s <- sd(values[seq(from = j, to = j + window - 1, by = 1)], na.rm = TRUE)
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
   for (i in seq_len(ncol(df_new))) {
      df_tmp <- data.frame(HP=df_new[, i], Group=rep(colnames(df_new)[i], length(df_new[, i]))) #nolint
      df_final <- rbind(df_final, df_tmp)
   }
   df_final
}


################################################################################
# get_time_diff
################################################################################
get_time_diff <- function(df, from = 2, to = 3) {
   id <- df %>% nth(1) %>% select("Animal No._NA")
   # note first time diff might be 0 if sorted ascending, thus pick 2 and 3 to check for consistency
   time_diff1 <- df %>% filter(`Animal No._NA` == id) %>% arrange(desc(diff.sec)) %>% nth(from) %>% select(diff.sec) %>% pull()
   time_diff2 <- df %>% filter(`Animal No._NA` == id) %>% arrange(desc(diff.sec)) %>% nth(to) %>% select(diff.sec) %>% pull()
   if (time_diff1 != time_diff2) {
      print("WARNING: Time difference different in cohorts!")
      print("This could happen if you do not average cohorts when sampling interval of IC experiments is different between cohorts")
      print("This could also happen if your single IC experiment data has been corrupted or has been recorded discontinously.")
      return(max(time_diff1, time_diff2) / 60)
   } else {
      return(time_diff1 / 60)
   }
}

################################################################################
# get_date_range
################################################################################
get_date_range <- function(df) {
 date_first <- df %>% select(Datetime) %>% first() %>% pull()
 date_last <- df %>% select(Datetime) %>% last() %>% pull()
 date_first <- paste(rev(str_split(str_split(date_first, " ")[[1]][1], "/")[[1]]), collapse = "-")
 date_last <- paste(rev(str_split(str_split(date_last, " ")[[1]][1], "/")[[1]]), collapse = "-")
 return(list("date_start" = date_first, "date_end" = date_last))
}


################################################################################
# check_for_cosmed
################################################################################
check_for_cosmed <- function(file) {
   if (length(excel_sheets(file)) == 2) {
        if ((excel_sheets(file)[1] == "Data") && (excel_sheets(file)[2] == "Results")) {
            first_col <- read_excel(file) %>% select(1)
            FIELDS_TO_CHECK <-  data.frame(index = c(1, 2, 3, 4, 5, 6), value = c("Last Name", "First Name", "Gender", "Age", "Height (cm)", "Weight (kg)"))
            return(all(apply(FIELDS_TO_CHECK, 1, function(x, output) return(x[2] == (first_col %>% nth(as.integer(x[1])) %>% pull())))))
        }
   }
}

################################################################################
# calc_heat_production
################################################################################
calc_heat_production <- function(choice, C1, variable, scaleFactor) {
   df <- C1
   switch(choice,
   Lusk = {
      df[[variable]] <- 15.79 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 5.09 * C1$RER_NA / 1000
   },
   HP = {
      df[[variable]] <- scaleFactor * C1$`VO2(3)_[ml/h]` * (6 * C1$RER_NA + 15.3) * 0.278 / 1000 * (3600 / 1000)
   },
   HP2 = {
      df[[variable]] <- (4.44 + 1.43 * C1$RER_NA) * scaleFactor * C1$`VO2(3)_[ml/h]` * (3600 / 1000) / 1000
   },
   Weir = {
      df[[variable]] <- 16.3 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.57 * C1$`VCO2(3)_[ml/h]` / 1000
   },
   Elia = {
      df[[variable]] <- 15.8 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 5.18 * C1$RER_NA / 1000
   },
   Brower = {
      df[[variable]] <- 16.07 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.69 * C1$RER_NA / 1000
   },
   Ferrannini = {
      df[[variable]] <- 16.37117 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.6057 * C1$RER_NA / 1000
   },
   {

   }
   )
   return(df)
}