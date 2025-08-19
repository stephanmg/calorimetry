library(DT)
source("inc/statistics/lme_model.R")

################################################################################
#' raw_measurement
#' 
#' This function displays raw measurements
#' @param finalC1 input data
#' @param finalC1meta combined metadata
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param global_data dictionary to store variables session-based for users
#' @param scaleFactor used to scale energy expenditure units correctly
#' @examples 
#' raw_measurement(values, full_metadata, input, output, session, global_data, 1)
#' @export
################################################################################
raw_measurement <- function(finalC1, finalC1meta, input, output, session, global_data, scaleFactor) {
	# get metadata
	metadatafile <- get_metadata_datapath(input, session, global_data)

	# only join data frame if not already joined 
	if (!is.null(getSession(session$token, global_data)[["is_Raw_calculated"]])) {
		data_and_metadata <- getSession(session$token, global_data)[["Raw_df"]]
		finalC1 <- data_and_metadata$data
		true_metadata <- data_and_metadata$metadata
	} else {
		finalC1$Animals <- as.factor(`$`(finalC1, "Animal No._NA"))
		data_and_metadata <- enrich_with_metadata(finalC1, finalC1meta, input$havemetadata, metadatafile)
		finalC1 <- data_and_metadata$data
		true_metadata <- data_and_metadata$metadata
		storeSession(session$token, "Raw_df", data_and_metadata, global_data)
	}

	# account for inconsistenly formatted data frames from TSE export
	colnames(finalC1) <- gsub("Â°C", "°C", colnames(finalC1))	

	# Select sexes
	if (!is.null(input$checkboxgroup_gender)) {
		# select male or female
		if ("Sex" %in% names(finalC1)) {
			finalC1 <- finalC1 %>% filter(Sex %in% c(input$checkboxgroup_gender))
		}
	}


	# Filter conditions based on factor level
	if (input$with_grouping) {
		my_var <- input$condition_type
		if (!is.null(input$select_data_by) && !is.null(input$condition_type)) {
			finalC1 <- finalC1 %>% filter((!!sym(my_var)) == input$select_data_by)
		}
	}

	# default from UI for light cycle start 
	light_on <- input$light_cycle_start 
	light_off <- input$light_cycle_stop

	# in case we have metadata, override with values from sheet
	if (input$havemetadata) {
		light_on <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_on"))) %>% select(2) %>% pull())
		light_off <- as.integer(get_constants(metadatafile) %>% filter(if_any(everything(), ~str_detect(., "light_off"))) %>% select(2) %>% pull())
	}

	# in case no information in metadata sheet, override light cycle manually
	if (input$override_metadata_light_cycle) {
		light_on <- input$light_cycle_start
		light_off <- input$light_cycle_stop
	}

	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][2], ":00", sep = "")
	}

	correct_day_count2 <- function(df) {

		df2 <- df %>%
  group_by(Animals) %>%
  mutate(
    max_time = max(running_total.sec, na.rm = TRUE),
    DayCounter = floor(max_time / 86400),
    DayCount = floor(running_total.sec / 86400) + 1
  ) %>%
  # Keep only rows whose time is inside the last full day boundary
  filter(running_total.sec < DayCounter * 86400) %>%
  ungroup() %>%
  select(-max_time)


df3 <- df %>%
  group_by(Animals) %>%
  mutate(
    max_time = max(running_total.sec, na.rm = TRUE),
    DayCounter = floor(max_time / 86400),
    DayCount = floor(running_total.sec / 86400)
  ) %>%
  # Keep only rows whose time is inside the last full day boundary
  filter(running_total.sec >= DayCounter * 86400) %>%
  ungroup() %>%
  select(-max_time)


print(length(colnames(df2)))
print(length(colnames(df3)))


df_combined <- rbind(df2, df3)

return(df_combined)
	}

	correct_day_count <- function(df) {
		seconds_per_day <- 86400

return(df2 <- df %>%
  group_by(`Animal No._NA`) %>%
  mutate(
    StartTime  = first(running_total.sec),
    ElapsedSec = running_total.sec - StartTime,
    # Calculate how far ElapsedSec is from an integer number of days
    DayRemainder = ElapsedSec %% seconds_per_day,
    # Determine if within ±5% of 86400 sec
    DayCount = case_when(
      ElapsedSec < 0                  ~ NA_real_,
      abs(DayRemainder - seconds_per_day) / seconds_per_day <= 0.05 ~ floor(ElapsedSec / seconds_per_day) + 1,
      DayRemainder / seconds_per_day <= 0.05 ~ floor(ElapsedSec / seconds_per_day),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-StartTime, -ElapsedSec, -DayRemainder))
	}

	num_full_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
	# when zeitgeber time should be used  
	if (input$use_zeitgeber_time) {
		finalC1 <- zeitgeber_zeit(finalC1, light_off)
		num_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
		print("Num days:")
		print(num_days)
		if (input$only_full_days_zeitgeber) {
			finalC1 <- correct_day_count2(finalC1)
			num_days <- max(finalC1$DayCount, na.rm=TRUE)
			print("new num days:")
			print(num_days)
			num_full_days <- num_days
			finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour > 0, running_total.hrs.halfhour < (24*num_days))
		} else {
			num_full_days <- num_days + 1 # account for missing half days at the beginning and end
		}

		write.csv(finalC1, "debug.csv")
		#finalC1$DayCount <- ceiling((finalC1$running_total.hrs.halfhour / 24) + 1)

		print("daycount:")

		print(finalC1$DayCount)
		finalC1 <- correct_day_count2(finalC1)
		finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < 12, "Night", "Day")
	} else {
		num_full_days <- floor(max(finalC1$running_total.hrs.halfhour) / 24)
		finalC1$Datetime2 <- lapply(finalC1$Datetime, convert)
		finalC1$NightDay <- ifelse(hour(hms(finalC1$Datetime2)) * 60 + minute(hms(finalC1$Datetime2)) < (light_on * 60), "Day", "Night")
		finalC1$NightDay <- as.factor(finalC1$NightDay)
		finalC1 <- finalC1 %>% mutate(Datetime4 = as.POSIXct(Datetime, format = "%d/%m/%Y %H:%M")) %>% mutate(Datetime4 = as.Date(Datetime4)) %>% group_by(`Animal No._NA`) %>% mutate(DayCount = dense_rank(Datetime4)) %>% ungroup()
	}

	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	colors <- as.factor(`$`(finalC1, "Animal No._NA"))
	finalC1$Animals <- colors

	# format variable from UI to compatible TSE format
	mylabel <- paste0(input$myr, sep = "", "_[%]")

	# for activity data
	if (startsWith(input$myr, "XT")) {
		mylabel <- paste0(input$myr, sep="", "_[Cnts]")
		mylabel <- sub("\\+", ".", mylabel)
	}

	# Note that units are always the same, when we read data files, we scale appropriately to reach
	# always these consensus/default units as displayed throughout the UI in the web application
	if (startsWith(input$myr, "V")) { mylabel <- paste0(input$myr, sep = "", "(3)_[ml/h]") }

	# rename Temp
	if (startsWith(input$myr, "Temp")) { mylabel <- paste0(input$myr, sep = "", "_[°C]") }
	if (startsWith(input$myr, "TempL")) { mylabel <- paste0(input$myr, sep = "", "_[°C]") }
	if (startsWith(input$myr, "TempC")) { mylabel <- paste0(input$myr, sep = "", "_[°C]") }
	if (startsWith(input$myr, "Drink1")) { mylabel <- paste0(input$myr, sep = "", "_[ml]") }
	if (startsWith(input$myr, "Feed1")) { mylabel <- paste0(input$myr, sep = "", "_[g]") }
	if (startsWith(input$myr, "DistD")) { mylabel <- paste0(input$myr, sep = "", "_[cm]") }
	if (startsWith(input$myr, "DistK")) { mylabel <- paste0(input$myr, sep = "", "_[cm]") }

	# rename RER_NA to RER (but finalC1 still has RER_NA)
	if (startsWith(input$myr, "RER")) { mylabel <- "RER_NA" }
	if (startsWith(input$myr, "EE")) { mylabel <- "EE_[kcal/day]" }

	# annotate days and animals (already shifted by above correction)
	day_annotations <- annotate_zeitgeber_zeit(finalC1, 0, mylabel, input$with_facets)

	# rename RER_NA to RER (but finalC1 still has RER_NA)
	if (startsWith(input$myr, "RER")) { mylabel <- "RER" }
	if (startsWith(input$myr, "EE")) { mylabel <- "EE" }


	colnames(finalC1)[colnames(finalC1) == "WeightBody_[g]"] <- "WeightBody"
	if (startsWith(input$myr, "WeightBody")) {
		mylabel <- "WeightBody"
	}

	# annotations for days (old)
	#finalC1 <- day_annotations$df_annotated

	# create input select fields for animals and days
	days_and_animals_for_select <- get_days_and_animals_for_select_alternative(finalC1)
	days_and_animals_for_select$days <- seq(1, num_full_days, 1)

	print("days and animals:")
	print(days_and_animals_for_select)

	# set default for animals and selected days: typically all selected at the beginning
	selected_days <- getSession(session$token, global_data)[["selected_days"]]
	if (is.null(selected_days)) {
		output$select_day <- renderUI({
			selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = days_and_animals_for_select$days, multiple = TRUE)
		})
		selected_days = days_and_animals_for_select$days
		storeSession(session$token, "selected_days", selected_days, global_data)
	} else {
		output$select_day <- renderUI({
			selectInput("select_day", "Select day(s):", choices = days_and_animals_for_select$days, selected = selected_days, multiple = TRUE)
		})
	}
	selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
	if (is.null(selected_animals)) {
		output$select_animal <- renderUI({
			selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = days_and_animals_for_select$animals, multiple = TRUE)
		})
		selected_animals = days_and_animals_for_select$animals
		storeSession(session$token, "selected_animals", selected_animals, global_data)
	} else {
		output$select_animal <- renderUI({
			selectInput("select_animal", "Select animal(s):", choices = days_and_animals_for_select$animals, selected = selected_animals, multiple = TRUE)
		})
	}

	# filter for selected days and animals in data set
	selected_animals <- getSession(session$token, global_data)[["selected_animals"]]
	finalC1 <- finalC1 %>% filter(`Animal No._NA` %in% selected_animals)

	# trim times from end and beginning of measurements 
	if (input$curate) {
		finalC1 <- finalC1 %>% filter(running_total.hrs.halfhour >= min(running_total.hrs.halfhour) + input$exclusion_start, running_total.hrs.halfhour <= (max(finalC1$running_total.hrs.halfhour) - input$exclusion_end))
	}

	# prepare grouping of data frame
	finalC1 <- finalC1 %>% filter(DayCount %in% intersect(selected_days, levels(as.factor(finalC1$DayCount))))
	finalC1$NightDay <- ifelse((finalC1$running_total.hrs %% 24) < 12, "Night", "Day")
	finalC1 <- finalC1 %>% filter(NightDay %in% input$light_cycle)
	finalC1$NightDay <- as.factor(finalC1$NightDay)

	# Select temperature
	if (!is.null(input$select_temperature)) {
		if (input$select_temperature) {
			finalC1 <- finalC1[finalC1$`Temp_[°C]` >= (input$temperature_mean-input$temperature_deviation) & finalC1$`Temp_[°C]` <= (input$temperature_mean+input$temperature_deviation), ]
		}
	}

	df_to_plot <- finalC1

	# if we do not have metadata, this comes from some not-clean TSE headers
	if (!input$havemetadata) { df_to_plot$`Animal.No.` <- df_to_plot$Animals }

	# format labels for plot
	mylabel <- paste0(input$myr, sep = "", "_[%]")

	if (startsWith(input$myr, "XT")) {
		mylabel <- paste0(input$myr, sep="", "_[Cnts]")
		mylabel <- sub("\\+", ".", mylabel)
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	myvar <- input$myr
	if (startsWith(input$myr, "V")) {
		mylabel <- paste0(input$myr, sep = "", "(3)_[ml/h]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "DistD")) {
		mylabel <- paste0(input$myr, sep = "", "_[cm]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "DistK")) {
		mylabel <- paste0(input$myr, sep = "", "_[cm]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "Temp")) {
		mylabel <- paste0(input$myr, sep = "", "_[°C]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "TempL")) {
		mylabel <- paste0(input$myr, sep = "", "_[°C]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "TempC")) {
		mylabel <- paste0(input$myr, sep = "", "_[°C]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "Drink1")) {
		mylabel <- paste0(input$myr, sep = "", "_[ml]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "Feed1")) {
		mylabel <- paste0(input$myr, sep = "", "_[g]")
		names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	}

	if (startsWith(input$myr, "RER")) {
		mylabel <- "RER"
	}

	if (startsWith(input$myr, "EE")) {
		mylabel <- "EE"
	}

	colnames(df_to_plot)[colnames(df_to_plot) == "WeightBody_[g]"] <- "WeightBody"

	if (startsWith(input$myr, "WeightBody")) {
		mylabel <- "WeightBody"
	}

	names(df_to_plot)[names(df_to_plot) == mylabel] <- input$myr
	names(df_to_plot)[names(df_to_plot) == "RER_NA"] <- "RER"
	names(df_to_plot)[names(df_to_plot) == "EE_[kcal/day]"] <- "EE"

	# TODO: v0.5.0 - factor this out as utility or method, can be re-used in other panels after discussion
	# replot outlier removed data, only if toggled: outlier removal by selection
	if (input$toggle_outliers) {
		if (!is.null(getSession(session$token, global_data)[["reactive_data"]])) {
		df_to_plot <- getSession(session$token, global_data)[["reactive_data"]]()
		print("replotting done?!")
		print(nrow(df_to_plot))
		}
		# note, it is prohibited to do any other filtering when outliers removal is toggled on
	} else {
		storeSession(session$token, "reactive_data", reactiveVal(df_to_plot), global_data)
	}

	gam_model <- NULL
	grouped_gam <- NULL
	if (input$add_average_with_se) {
		if (input$with_facets) {
			if (!is.null(input$facets_by_data_one)) {
				signal <- input$myr
				group = input$facets_by_data_one
				# Fit GAM for each group
				grouped_gam <- df_to_plot %>%
				group_by(!!sym(group)) %>%
				group_map(~ {
					group_value <- .y[[group]][1]
					gam_model <- mgcv::gam(as.formula(paste(signal, " ~ s(running_total.hrs.halfhour, k = ", as.numeric(input$averaging_method_with_facets_basis_functions), ", bs = 'cr')")), data= .x)
					pred <- predict(gam_model, se.fit = TRUE)
					.x %>%
					mutate(
						fit = pred$fit,
						upper = pred$fit + input$averaging_method_with_facets_confidence_levels * pred$se.fit,
						lower = pred$fit - input$averaging_method_with_facets_confidence_levels * pred$se.fit,
						trend = group_value
					)
				}) %>%
				bind_rows()  # Combine predictions for all groups
			}
		} else {
			gam_model <- mgcv::gam(df_to_plot[[input$myr]] ~ s(running_total.hrs.halfhour, k=input$averaging_method_with_facets_basis_functions, bs=input$averaging_method_with_facets_basis_function), data=df_to_plot)
			pred <- predict(gam_model, se.fit=TRUE)
			df_to_plot <- df_to_plot %>% mutate(fit=pred$fit, upper = fit + input$averaging_method_with_facets_confidence_levels * pred$se.fit, lower = fit - input$averaging_method_with_facets_confidence_levels * pred$se.fit)
		}
	}

	p <- ggplot(data = df_to_plot, aes_string(y = input$myr, x = "running_total.hrs.halfhour", color = "Animals", group = "Animals")) + geom_line()
	mylabel <- gsub("_", " ", mylabel)

	# Add trend for ungrouped data
	if (input$add_trend_line) {
		if (!input$with_facets) {
			if (!is.null(input$trend_line_color_scale)) {
				if (input$override_color_scale_in_trendline) {
			  p <- p + scale_color_viridis_d(option = input$trend_line_color_scale) 
			  if (input$trend_line_color_scale == "black") {
				scale_color_black <- function(groups) {
 			 scale_color_manual(values = setNames(rep("black", length(groups)), groups))
			}
			p <- p + scale_color_black(unique(df_to_plot[["Animals"]]))
			  }
			}
			}
			summary_df <- df_to_plot %>% group_by(running_total.hrs.halfhour) %>% summarise(mean=mean(.data[[input$myr]], na.rm = TRUE), sd=sd(.data[[input$myr]], na.rm = TRUE), .groups="drop")
			p <- p + geom_line(data=summary_df, aes(x=running_total.hrs.halfhour, y=mean), color = "blue", inherit.aes=FALSE) 
			p <- p + geom_ribbon(data=summary_df, aes(x=running_total.hrs.halfhour, ymin=mean-input$add_trend_line_sd*sd, ymax=mean+input$add_trend_line_sd*sd), fill = "lightblue", alpha=0.6, inherit.aes=FALSE)
		} else {
			if (!is.null(input$facets_by_data_one)) {
			if (!is.null(input$trend_line_color_scale)) {
				if (input$override_color_scale_in_trendline) {
			  p <- p + scale_color_viridis_d(option = input$trend_line_color_scale) 
				  if (input$trend_line_color_scale == "black") {
				scale_color_black <- function(groups) {
 			 scale_color_manual(values = setNames(rep("black", length(groups)), groups))
			}
			p <- p + scale_color_black(unique(df_to_plot[["Animals"]]))
			  }
			}
			}
				
# Step 1: Create summary data
summary_df <- df_to_plot %>%
  group_by(running_total.hrs.halfhour, .data[[input$facets_by_data_one]]) %>%
  summarise(
    mean = mean(.data[[input$myr]], na.rm = TRUE),
    sd = sd(.data[[input$myr]], na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Prepare group and colors
group_var <- input$facets_by_data_one
groups <- unique(summary_df[[group_var]])

# Generate n distinct colors from the selected viridis scale
my_colors <- viridisLite::viridis(length(groups), option = input$trend_line_color_scale)

# Step 3: Add one line per group using a loop
for (i in seq_along(groups)) {
  grp <- groups[i]
  clr <- my_colors[i]

  sub_df <- summary_df[summary_df[[group_var]] == grp, ]

  # Add individual line with manually set color
  p <- p + geom_line(
    data = sub_df,
    aes(x = running_total.hrs.halfhour, y = mean),
    color = clr,
    inherit.aes = FALSE
  )
  p <- p + geom_ribbon(
	data = sub_df,
	aes(x = running_total.hrs.halfhour,
	ymin = mean - input$add_trend_line_sd*sd,
	ymax = mean + input$add_trend_line_sd*sd),
	fill = clr,
	alpha = 0.6,
	inherit.aes = FALSE
  )
}
			}
		}
	}

	# annotate timeline
	lights <- data.frame(x = df_to_plot["running_total.hrs.halfhour"], y = df_to_plot[input$myr])
	colnames(lights) <- c("x", "y")
	if (input$timeline) {
		if (!is.null(input$only_full_days_zeitgeber)) {
			if (input$only_full_days_zeitgeber == TRUE) {
				my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle, input$only_full_days_zeitgeber)
				p <- p + my_lights
			} else {
				my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle)
				p <- p + my_lights
			}
		}
	}

	# set y-axis labels 
	p <- p + ylab(pretty_print_variable(mylabel, metadatafile))

	# set x-axis label
	if (input$use_zeitgeber_time) {
		p <- p + xlab("Zeitgeber time [h]")
	} else {
		p <- p + xlab("Time [h]")
	}

	# helper to convert datetime format
	convert <- function(x) {
		splitted <- strsplit(as.character(x), " ")
		paste(splitted[[1]][1], "", sep = "")
	}

	p2 <- NULL
	# df to plot now contains the summed oxidation over individual days   
	df_to_plot$Datetime <- day(dmy(lapply(df_to_plot$Datetime, convert)))
	annotations_window_plot <- NULL
	# show either time trace or a windows time trace, and feed respective data frames into ANOVA/ANCOVA panel
	if (input$windowed_plot == FALSE) {
		# regular time trace plot
		df_to_plot$raw_df = df_to_plot[input$myr]
		raw_df <- df_to_plot %>% group_by(Animals, DayCount) %>% summarize(raw_df = (max(raw_df, na.rm=TRUE)+min(raw_df, na.rm=TRUE))/2, .groups = "drop") %>% rename("Days"=DayCount) %>% rename(TEE=raw_df)

		# add cohort information
		interval_length_list <- getSession(session$token, global_data)[["interval_length_list"]]
		raw_df$Cohort <- sapply(raw_df$Animals, lookup_cohort_belonging, interval_length_list_per_cohort_and_animals=interval_length_list)

		# store calculated results
		storeSession(session$token, "df_raw", raw_df, global_data)
		storeSession(session$token, "selected_indep_var", "Genotype", global_data)
		
		# add anova/ancova panel
		add_anova_ancova_panel(input, output, session, global_data, true_metadata, raw_df, metadatafile, mylabel, "Raw")
	} else {
		# offset is minimum value for time (on x-axis)
		offset <- min(finalC1$running_total.hrs.halfhour)

		# windowed time trace plot
		window_plot <- add_windowed_plot(input, output, session, global_data, true_metadata, metadatafile, df_to_plot, "Raw", offset, input$myr)
		p2 <- window_plot$plot
		p2 <- p2 + ggtitle(paste0("Average measurement of ", mylabel, " in window")) + ylab(mylabel)
		annotations_window_plot <<- window_plot$annotations
	}
	
	# add facetting
	if (input$with_facets) {
		if (!is.null(input$facets_by_data_one)) {
			if (input$orientation == "Horizontal") {
				p <- p + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
				if (!is.null(input$facet_medians)) {
					if (!input$facet_medians) {
						p2 <- p2 + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
					} else {
						if (!is.null(input$facet_medians_in_one_plot)) {
							if (!input$facet_medians_in_one_plot) {
								p2 <- p2 + facet_grid(as.formula(paste(".~", input$facets_by_data_one)), scales="free_x")
							}
						}
					}
				}
			} else {
				p <- p + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
				if (!is.null(input$facet_medians)) {
					if (!input$facet_medians) {
						p2 <- p2 + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
					} else {
						if (!is.null(input$facet_medians_in_one_plot)) {
							if (!input$facet_medians_in_one_plot) {
								p2 <- p2 + facet_grid(as.formula(paste(input$facets_by_data_one, "~.")), scales="free_y")
							}
						}
					}
				}
			}
		}
	}

	# average for original time trace plot
	if (input$add_average_with_se) {
		if (input$with_facets) {
			if (!is.null(input$facets_by_data_one)) {
				grouped_gam$trend <- as.factor(grouped_gam$trend)
				if (!is.null(input$add_average_with_se_one_plot)) {
					if (input$add_average_with_se_one_plot) {
						p <- ggplot(data = df_to_plot, aes_string(y = input$myr, x = "running_total.hrs.halfhour"))
						p <- p + geom_ribbon(data = grouped_gam, aes(ymin = lower, ymax = upper, group = trend, color=trend, fill=trend), alpha =input$averaging_method_with_facets_alpha_level) 
						p <- p + labs(colour=input$facets_by_data_one, fill=input$facets_by_data_one)
						# set y-axis label
						mylabel <- gsub("_", " ", mylabel)
						p <- p + ylab(pretty_print_variable(mylabel, metadatafile))
						# set x-axis label
						if (input$use_zeitgeber_time) {
							p <- p + xlab("Zeitgeber time [h]")
						} else {
							p <- p + xlab("Time [h]")
						}
						# add back timeline
						if (input$timeline) {
							if (!is.null(input$only_full_days_zeitgeber)) {
								if (input$only_full_days_zeitgeber == TRUE) {
									my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle, input$only_full_days_zeitgeber)
									p <- p + my_lights
								} else {
									my_lights <- draw_day_night_rectangles(lights, p, light_on, light_off, 0, input$light_cycle_day_color, input$light_cycle_night_color, input$light_cycle)
									p <- p + my_lights
								}
							}
						}
					} else {
						p <- p + geom_ribbon(data = grouped_gam, aes(ymin = lower, ymax = upper, group = trend, color=trend, fill=trend), alpha =input$averaging_method_with_facets_alpha_level) 
					}
				}
			}
		} else {
				p <- p + geom_ribbon(aes(ymin=lower, ymax=upper), alpha=input$averaging_method_with_facets_alpha_level, fill=input$averaging_method_with_facets_color)
		}
	}

	if (input$add_trend_line_one_plot) {
		# TODO: this is not enough, we want instead to add the grouped df directly to have also legend entries,
		# individual geom_line(...) will not result in legend entries...
 	 	p <- ggplot() 
				summary_df <- df_to_plot %>% group_by(running_total.hrs.halfhour, .data[[input$facets_by_data_one]]) %>% summarise(mean=mean(.data[[input$myr]], na.rm = TRUE), sd=sd(.data[[input$myr]], na.rm = TRUE))
				p <- p + geom_line(data=summary_df, aes(x=running_total.hrs.halfhour, y=mean, color=.data[[input$facets_by_data_one]], group=.data[[input$facets_by_data_one]]),  inherit.aes=FALSE)
				p <- p + geom_ribbon(
 		 	   		data = summary_df,
					aes(
						x = running_total.hrs.halfhour,
						ymin = mean - input$add_trend_line_sd*sd,
						ymax = mean + input$add_trend_line_sd*sd,
					fill = .data[[input$facets_by_data_one]],
						group = .data[[input$facets_by_data_one]]
						),
						alpha = 0.6,
						inherit.aes = FALSE
					)
		if (!is.null(input$trend_line_color_scale)) {
				if (input$override_color_scale_in_trendline) {
			  p <- p + scale_color_viridis_d(option = input$trend_line_color_scale) 
			  if (input$trend_line_color_scale == "black") {
				scale_color_black <- function(groups) {
 			 scale_color_manual(values = setNames(rep("black", length(groups)), groups))
			}
			p <- p + scale_color_black(unique(df_to_plot[["Animals"]]))
			  }
				}
		}


	}


	# if we have full days based on zeitgeber time, we kindly switch to Full Day annotation instead of Day
	if (input$only_full_days_zeitgeber) {
		day_annotations$annotations <- day_annotations$annotations %>% mutate(label=gsub("Day", "Full Day", label))
	}

	# need to start at 0 and 12 for zeitgeber time
	light_offset <- -12 # otherwise outside 0 on left
	first_night_start = 0 # always 0 in zeitgeber time

	# add day annotations and indicators vertical lines
	# +2 for annotation inside plotting
	p <- p + geom_text(data=day_annotations$annotations, aes(x = x+light_offset+2.5+first_night_start, y = y+6, label=label), vjust=1.5, hjust=0.5, size=3, color="black")
	# indicate new day
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+24+first_night_start, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="black")
	# indicate night start
	p <- p + geom_vline(xintercept = as.numeric(seq(light_offset+12+first_night_start, length(unique(days_and_animals_for_select$days))*24+light_offset, by=24)), linetype="dashed", color="gray")
	# set title and display buttons
	p <- p + ggtitle(paste0("Raw measurement: ", pretty_print_variable(mylabel, metadatafile), " using equation ", pretty_print_equation(input$variable1)))
	# add points only if toggle outliers
	if (input$toggle_outliers) { p <- p + geom_point() }
	# center x axis
	p <- p + scale_x_continuous(expand = c(0, 0), limits = c(min(finalC1$running_total.hrs.halfhour), max(finalC1$running_total.hrs.halfhour)))
	if (input$boxplots_or_sem_plots == FALSE) {
		p2 <- p2 + scale_x_continuous(expand = c(0, 0), limits = c(min(finalC1$running_total.hrs.halfhour), max(finalC1$running_total.hrs.halfhour)))
	}
	# scale y axis to range 
	y_range <- range(df_to_plot[[input$myr]], na.rm = TRUE)
	p <- p + scale_y_continuous(limits=y_range)

	p2 <- p2 + annotations_window_plot
	# toggle outliers
	if (input$toggle_outliers) {
		exceed_indices <- which(df_to_plot[[input$myr]] > input$threshold_toggle_outliers)
		p <- ggplotly(p)
		for (i in seq_along(exceed_indices)) {
			p <- p %>% add_segments(x = df_to_plot$running_total.hrs.halfhour[exceed_indices[i]]-0.25, xend = df_to_plot$running_total.hrs.halfhour[exceed_indices[i]]+0.25, y = input$threshold_toggle_outliers, yend = input$threshold_toggle_outliers, line = list(color="red", width=8), name = paste("Outlier #", i)) # showlegend=FALSE
		}
	}
	# store number of total curves already present in plotly
	storeSession(session$token, "all_curves_plotly", length(plotly_build(p)$x$data), global_data)
	p <- ggplotly(p) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
	if (input$windowed_plot == TRUE) {
		if (!is.null(p2)) {
			p2 <- ggplotly(p2) %>% config(displaylogo = FALSE, modeBarButtons = list(c("toImage", get_new_download_buttons()), list("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"), list("hoverClosestCartesian", "hoverCompareCartesian")))
		}
	}

	# create LME model UI
	create_lme_model_ui(input, output, true_metadata, df_to_plot, input$myr, session, global_data)

	# store plot and indicate that Raw has been calculated
	plots_for_raw <- getSession(session$token, global_data)[["plot_for_raw"]]
	if (is.null(plots_for_raw)) {
		plots_for_raw <- list()
	} 
	plots_for_raw[[input$myr]] <- p
	
	storeSession(session$token, "plot_for_raw", plots_for_raw, global_data)
	storeSession(session$token, "plot_for_raw_window", p2, global_data)
	storeSession(session$token, "is_Raw_calculated", TRUE, global_data)
	storeSession(session$token, "is_Raw_window_calculated", length(p2) > 0, global_data)
	storeSession(session$token, "raw_variable", input$myr, global_data)

	# return current plot of raw measurements
	return(list("window_plot"=p2, "plot"=p))
}
