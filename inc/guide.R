library(shiny)
library(cicerone)

guide <- Cicerone$
    new(id = "guide")$
    step(
        "heat_production_equations",
        "Heat production equations",
        "Choose two desired heat production equations from above and corresponding units (kJ or kcal)"
    )$
    step(
        "havemetadata",
        "Additional metadata",
        "Provide additional metadata via Metadata sheet"
    )$
    step(
        "nFiles",
        "Number of data files",
        "Specify how many data files you wish to analyze"
    )$
    step(
        "negative_values",
        "Plausability check",
        "Indicate if you wish to filter out negative values in raw measurements"
    )$
    step(
        "highly_varying_measurements", 
        "Data curation",
        "Indicate if you wish to filter out highly (unphysiologically) varying raw measurements" 
    )$
    step(
        "plotting",
        "Show plot"
    )$
    step(
        "reset",
        "Reset plot"
    )$
    step(
        "with_grouping",
        "Group and filter",
        "Group by selected groups"
    )$
    step(
        "timeline",
        "Timeline",
        "Color code night/day light cycle"
    )$
    step(
        "outliers",
        "Outliers",
        "Remove outliers, or remove time ranges (see above)"
    )$
    step(
        "export_file_name",
        "Save data",
        "Choose a file name to save compiled data sets"
    )