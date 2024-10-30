library(shiny)
library(cicerone)

################################################################################
#' guide
#' 
#' This function creates a simple user guide navigating the user through the app
################################################################################
guide <- Cicerone$
    new(id = "guide")$
    step(
        "heat_production_equations",
        "Heat production equations",
        "Choose your desired heat production equation from the list above and select the unit (kJ or kcal) for energy expenditure."
    )$
    step(
        "havemetadata",
        "Metadata for data sets",
        "Provide additionally metadata via the standardized Metadata sheet or use the provided metadata in the TSE file header"
    )$
    step(
        "nFiles",
        "Number of data files",
        "Specify how many data sets (cohorts) you wish to upload and analyze"
    )$
    step(
        "negative_values",
        "Plausability check",
        "Indicate if you wish to automatically filter out e.g. negative values or inconsistent raw measurements"
    )$
    step(
        "highly_varying_measurements",
        "Data curation",
        "Indicate if you wish to filter out highly (unphysiologically) varying raw measurements"
    )$
    step(
        "plotting",
        "Show plot",
        "Plotting control lets you (re-)plot your data",
    )$
    step(
        "reset",
        "Reset plot",
        "Plotting control lets you reset the plots and individual user session, indicated by the token reported in the bottom left corner."
    )$
    step(
        "with_grouping",
        "Group and filter",
        "Group data by provided level, e.g. condition, diet or treatment"
    )$
    step(
        "timeline",
        "Timeline",
        "Shade plot by light or dark phase to indicate day and night times"
    )$
    step(
        "outliers",
        "Outliers",
        "Remove specific samples identified as outliers manually",
    )$
    step(
        "export_file_name",
        "Save data",
        "Choose a file name to save compiled data sets and calculated quantities. Plotting data frame can be exported as individual .csv files below as well."
    )