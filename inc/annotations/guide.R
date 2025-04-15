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
        "example_data_single",
        "Example data set UCP1 KO",
        "Choose example data set I to explore app features"
    )$
    step(
        "example_data_single_alternative",
        "Example data set DAKO",
        "Choose example data set II to explore app features"
    )$
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
        "use_raw_data_curation",
        "Raw data curation",
        "Indicate if you wish to automatically filter out e.g. negative values or inconsistent raw measurements"
    )$
    step(
        "plotting",
        "Show plot",
        "Plotting control lets you (re-)plot your data",
    )$
    step(
        "refresh",
        "Refresh the plot",
        "Plott might need manualy refreshing in some cases, e.g. Browser window resize"
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
    )$
    step(
        "session_id",
        "Session information",
        "Provide information of current user session during a bug report"
    )$
    step("plot",
        "Main plot",
        "The basic plot is the starting point of analyses. A scatter plot over time with the measured or calculated quantity or a boxplot of measurements or calculated quantity per Day is plotted."
    )