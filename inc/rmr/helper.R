library(dplyr)
library(ggplot2)
library(shinyalert)

################################################################################
# padding_helper
################################################################################
padding_helper <- function(df) {
   # Find the last row for each group
df_max_index <- df %>%
  group_by(Group) %>%
  slice(n()) %>%
  ungroup()

# Function to insert a row after the max index for each group
insert_row <- function(data, row, after) {
  data <- rbind(data[1:after, ], row, data[(after + 1):nrow(data), ])
  return(data)
}

# Initialize a new data frame to store the results
new_df <- df

# Loop through each group to insert the new row
for (i in seq_len(nrow(df_max_index))) {
  row_to_insert <- df_max_index[i, ]
  group_rows <- which(df$Group == df_max_index$Group[i])
  max_index <- max(group_rows)
  new_df <- insert_row(new_df, row_to_insert, max_index)

}
return(new_df)
}


################################################################################
# partition
################################################################################
partition <- function(mydf) {
   df <- mydf
   data <- df %>% group_split(Group)
   df_new <- data.frame()
   new_col_names <- c()
   m <- max(sapply(data, nrow))
   for (i in data) {
      new_col_names <- append(new_col_names, unique(i$Group))
      diff <- m - length(i$Values)
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
            df_new <- cbind(df_new, c(i$Values))
         }
      }
   }
   colnames(df_new) <- new_col_names
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
      shinyalert("Error", "Time difference different (measurement interval CHANGING) in cohort for animals. Check your data files. All measurement intervals should be constant per individual cohort (and thus for each animal in the cohort). Measurement intervals can vary between cohorts, which is valid input to the analysis.", type = "warning", showCancelButton = TRUE)
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
      df[[variable]] <- 16.37117 * scaleFactor * C1$`VO2(3)_[ml/h]` / 1000 + 4.6057 * C1$`VCO2(3)_[ml/h]` / 1000
   },
   {

   }
   )
   return(df)
}

################################################################################
# filter_full_days
################################################################################
convert_to_days <- function(x) {
   splitted <- strsplit(as.character(x), " ")
   paste(splitted[[1]][1])
}

filter_full_days_alternative <- function(df, threshold) {
   return(df %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(Datetime4) %>% filter(n_distinct(hour) >= (24 * (100/(100-threshold)))) %>% ungroup())
}

filter_full_days <- function(df, time_diff, threshold) {
   df$DaysCount <- lapply(df$Datetime, convert_to_days)
   df$`Animal No._NA` <- as.factor(df$`Animal No._NA`)
   splitted <- df %>% group_by(`Animal No._NA`) %>% group_split(`Animal No._NA`)
   ls <- c()
   for (s in splitted) { # for each animal
      ls <- append(ls, lapply(s %>% group_split(DaysCount), nrow)) # count hours for days
   }

   df_final <- NULL
   for (s in splitted) {
      df_part <- s %>% group_by(DaysCount) %>% mutate(FullDay = length(`Animal No._NA`))
      df_part <- df_part %>% filter(FullDay > (threshold / 100) * 60 * 24 / time_diff)
      df_final <- bind_rows(df_final, df_part)
   }
   df_final <- df_final %>% select(-c("FullDay"))
   df_final <- df_final %>% ungroup()
   df_final <- df_final %>% select(-c("DaysCount"))
   return(df_final)
}


################################################################################
# trim_front_end
################################################################################
convert_to_day_only <- function(x) {
   splitted <- strsplit(as.character(x), "/")
   paste(splitted[[1]][1], splitted[[1]][2], splitted[[1]][3], sep = "-")
}

trim_front_end <- function(df, end_trim, front_trim) {
   df$Date <- lapply(df$Datetime, convert_to_days)
   df$Date <- lapply(df$Date, convert_to_day_only)
   splitted <- df %>% group_by(`Animal No._NA`) %>% group_split(`Animal No._NA`)
   df_final <- NULL

   for (s in splitted) {
      last_row <- s %>% arrange(Date) %>% nth(nrow(s)) %>% select(Date) %>% pull() # assumed last date for animal
      first_row <- s %>% arrange(Date) %>% nth(1) %>% select(Date) %>% pull() # assumed first date for animal

      hours_start <- s %>% filter(Date == first_row[[1]][1]) %>% select(hour) %>% unique() %>% nth(1) %>% pull()

      hours_end <- s %>% filter(Date == last_row[[1]][1]) %>% select(hour) %>% unique() %>% last() %>% pull()

      df_filtered <- s %>% filter(!((Date == last_row[[1]][1] & hour > (hours_end - end_trim)) | (Date == first_row[[1]][1] & hour < (hours_start + front_trim))))
      df_filtered <- df_filtered %>% ungroup()
      df_filtered <- df_filtered %>% arrange(Datetime2)
      df_filtered <- df_filtered %>% select(-c("Date"))
      df_final <- bind_rows(df_final, df_filtered)
   }
   return(df_final)
}
