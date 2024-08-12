library("shinyFiles")
library("plotly")
library("shinybusy")
library("shinythemes")
library("shinyWidgets")
library("shinyhelper")
library(shinyjs)
library("colourpicker")
library(cicerone)

################################################################################
# First page: Introduction
################################################################################
intro_panel <- tabPanel(
  "Introduction and Features",
  titlePanel("A reactive web-based application for the analysis of indirect calorimetry experiments"),
  tags$img(src = "splash.jpg", align = "right"),
  p("A R Shiny web app for the analysis of indirect calorimetric data provided in standardized data formats from the TSE Phenomaster/Phenolab, Sable Promethion and Columbus Instruments CLAMS system."), #nolint
  p("In this document the analysis of data acquired during indirect calorimetry experiments is explained for: "),
   tags$ul(
   tags$li("long term observations over multiple hours"),
   tags$li("short therm/acute response experiments < 2 hours")
   ),
  h3("Supported input file formats"),
  tags$table(class = "fileformat-types",
   tags$tr(
      tags$td(icon("file-excel")),
      tags$td(icon("minus")),
      tags$td("Promethion/Sable")
   ),
   tags$tr(
      tags$td(icon("file-excel")),
      tags$td(icon("minus")),
      tags$td("COSMED")
   ),
   tags$tr(
      tags$td(icon("file-csv")),
      tags$td(icon("minus")),
      tags$td("TSE PhenoMaster v7")
   ),
   tags$tr(
      tags$td(icon("file-csv")),
      tags$td(icon("minus")),
      tags$td("TSE PhenoMaster v8")
   ),
   tags$tr(
      tags$td(icon("file-csv")),
      tags$td(icon("minus")),
      tags$td("TSE LabMaster v6")
   ),
   tags$tr(
      tags$td(icon("file-csv")),
      tags$td(icon("minus")),
      tags$td("TSE LabMaster v5")
   )
   ),
   br(),
   p("For a non-exhaustive list of features, see the section below. Use top navigation bar to get started."),
   hr(style = "width:75%;"),
   h1("List of selected app features"),
   tags$table(class = "feature-table",
      tags$tr(
         tags$td(icon("timeline", "fa-3x")),
         tags$td(h3("Long term observational studies")),
         tags$td(p("On a single data set. This description deals with an animal experiment investigating mice of two different genotypes (ko & wt) running in two cohorts (C1 & C2) and thus at two different timepoints. The provided commands are compatible with csv-files exported by the LabMaster software. Just export the respective csv-file and save it to a appropriate location.")) #nolint
      ),
      tags$tr(
         tags$td(icon("plus-minus", "fa-3x")),
         tags$td(h3("Time averaging for robust analysis")),
         tags$td(p("As calorimetry data is quite unstable, it is usually averaged over a certain time period (e.g. 1h as in CalR). However, one might to adjust this time period to personal preferences e.g. 30 minute intervals. This script offers this possibility. In order to do so, one must again extract some additional information form the data. In a first step, the cumulative running time of the experiment is calculated. The cumulative time (in s) is subsequently transformed in hours.")) #nolint

      ),
      tags$tr(
         tags$td(icon("user-xmark", "fa-3x")),
         tags$td(h3("Outlier detection and removal")),
         tags$td(p("Although not recommended you might want to exclude animals from the data. Maybe animals got sick, did not eat or technical issues with the cage occurred. One solution to remove whole animals is provided below.")) #nolint
      ),
      tags$tr(
         tags$td(icon("folder-tree", "fa-3x")),
         tags$td(h3("Combining several data sets")),
         tags$td(p("Limited capacity is often an issue for calorimetry, thus mice might be measured at different time points and need to be combined to a single data set. This is a issue since merging by time is not that easy due to slightly different starting time points etc. Be sure to always adjust your time period - the date can/should differ, however your starting period should always be in the same hour (e.g. 12:02:00 for the first data set and 12:00:00 for the second data set)")) #nolint
      ),
      tags$tr(
         tags$td(icon("magnifying-glass-chart", "fa-3x")),
         tags$td(h3("Visualization and analysis of data")),
         tags$td(p("Head over to the Visualization tab in the navigation bar at the top of this window - currently single data files are supported for analysis")) #nolint
      ),
      tags$tr(
         tags$td(icon("file-export", "fa-3x")),
         tags$td(h3("Data export")),
         tags$td(p("Various export data formats supported, for instance CalR compatible data export"))
      )
   ),
   hr(style = "width:100%;")
)

################################################################################
# side bars
################################################################################
sidebar_content2 <- sidebarPanel(
   fileInput("rerself", "RER self (means)"),
   fileInput("rercalr", "RER CalR (means)"),
   h1("Plotting control"),
   textInput("plotTitle", "Plot title", paste0("Average of RER value")),
   actionButton("plottingvalidation", "Show plots"),
   actionButton("reset", "Reset session"),
)

sidebar_content3 <- sidebarPanel(
   numericInput("nFiles", "Number of files", value = 1, min = 1, step = 1),
   uiOutput("fileInputs"),
)

# Note: Should change to table format for a better visual alignment of components
sidebar_content <- sidebarPanel(
   fluidPage(
   fluidRow(
      column(8, style = "padding: 0px;",
      h1("Configuration"),
      br(),
      actionButton("guide", "Guide (Click me)", style = "border: 1px solid white; background-color: rgba(255,69,0,0.5)"),
      br(), br()
   ),
   column(2, style = "padding: 20px;",
    actionButton("showTabHP", label = "", icon = icon("square-plus", "fa-3x"))
   ),
   column(2, style = "padding: 20px;",
    actionButton("hideTabHP", label = "", icon = icon("square-minus", "fa-3x"))
   )),
   tabsetPanel(id = "tabsHP", type = "hidden",
      tabPanelBody("HP",
   add_busy_bar(color = "#0FFF50"), # #50C878, # #AAFF00
   withMathJax(),
   h3("Energy expenditure equation"),
   conditionalPanel("input.plot_type != 'CompareHeatProductionFormulas'", selectInput("variable1", "Select equation", choices = c("HP2", "HP", "Weir", "Ferrannini"))),
   #conditionalPanel("input.plot_type != 'CompareHeatProductionFormulas'", selectInput("variable1", "Select equation", choices = c("HP2", "HP", "Lusk", "Weir", "Elia", "Brouwer", "Ferrannini"))),
   conditionalPanel("input.plot_type == 'CompareHeatProductionFormulas'", selectInput("variable1", "Select first equation", choices = c("HP", "HP2", "Lusk", "Weir", "Elia", "Brouwer", "Ferrannini"))),
   conditionalPanel("input.plot_type == 'CompareHeatProductionFormulas'", selectInput("variable2", "Select second equation", choices = c("HP2", "HP", "Lusk", "Weir", "Elia", "Brouwer", "Ferrannini"))),
   selectInput("kj_or_kcal", "Unit of energy", choices = c("kJ", "kcal")),
   withMathJax(),
   tags$head(
      tags$script(type = "text/x-mathjax-config", HTML(
         'MathJax.Hub.Config({
         TeX: {
            equationNumbers: {
               autoNumber: "AMS",
               formatNumber: function (n) { return "[" + n + "]"; }
               }
            }
            });'
         )
      )
   ),
   uiOutput("heat_production_equations"),
   h3("Metadata"),
   div("Provide by a standardized Metadatasheet (7) in Excel format"),
   checkboxInput(inputId = "havemetadata", label = "Have additional metadata?"),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("metadatafile")),
   h3("Data sets"),
   p("Use the file choser dialog to select individual file(s) for analysis"),
   numericInput("nFiles", "Number of data files", value = 1, min = 1, step = 1),
   uiOutput("fileInputs"),
   h4(textOutput("additional_information")),
   span(textOutput("file_type_detected"), style = "color:green; font-weight: bold;"),
   span(textOutput("study_description"), style = "color:orange; font-weight: bold;"),
   h3("Preprocessing"),
   checkboxInput(inputId="coarsen_data_sets", "Coarsen data sets"),
   conditionalPanel(condition = "input.coarsen_data_sets == true", numericInput("coarsening_factor", "Factor", value = 1, min = 1, max = 10, step=1)),
   h3("Raw data curation"),
   checkboxInput(inputId = "z_score_removal_of_outliers", label = "Remove outliers by z-score"),
   conditionalPanel(condition = "input.z_score_removal_of_outliers == true", numericInput("sds", "Number of SDs", value = 2, step=1, min = 0, max = 3)),
   conditionalPanel(condition = "input.z_score_removal_of_outliers == true", selectInput("target_columns", "Measurements", c("VO2", "VCO2"), multiple=TRUE, selected=c("VO2", "VCO2"))),
   checkboxInput(inputId = "remove_zero_values", label = "Remove zero values"),
   conditionalPanel(condition = "input.remove_zero_values == true", numericInput("eps", "Epsilon", value=1e-6, min=1e-9, max=1e-3, step=1e-3)),
   checkboxInput(inputId = "toggle_outliers", "Manually mark outliers above threshold", value = FALSE),
   conditionalPanel("input.toggle_outliers == true", numericInput(inputId = "threshold_toggle_outliers", "Threshold", value=20.67, min = 0, max = 100, step = 0.01)),
   checkboxInput(inputId = "toggle_lasso_outliers", "Select outliers by box selection"), 
   conditionalPanel("input.toggle_lasso_outliers == true", actionButton("remove_lasso_points", "Remove lasso selection")),
   h3("Raw data consistency checks"),
   checkboxInput(inputId = "negative_values", label = "Detect negative values", value = FALSE),
   checkboxInput(inputId = "detect_nonconstant_measurement_intervals", label = "Detect non-constant measurement intervals", value = FALSE),
   checkboxInput(inputId = "highly_varying_measurements", label = "Detect highly varying measurements", value = FALSE),
   conditionalPanel("input.highly_varying_measurements == true", sliderInput("threshold_for_highly_varying_measurements", "Threshold [%]", min = 0, max = 200, step = 10, value = 200)),
   h3("Plotting controls"),
   actionButton("plotting", "Show"),
   actionButton("reset", "Reset"),
   ))),
   hr(),
   fluidPage(
   fluidRow(
      column(8, style = "padding: 0px;",
      h1("Plotting")),
   column(2, style = "padding: 20px;",
    actionButton("showTabPC", label = "", icon = icon("square-plus", "fa-3x")),
   ),
   column(2, style = "padding: 20px;",
    actionButton("hideTabPC", label = "", icon = icon("square-minus", "fa-3x")),
   ))),
   tabsetPanel(id = "tabsPC", type = "hidden",
      tabPanelBody("PC",
   selectInput(inputId = "ic_system", "Select indirect calorimetry platform", factor(c("General", "COSMED", "Sable"))),
   conditionalPanel(condition = "input.ic_system == 'General'", selectInput("plot_type", "Select quantity to plot", factor(c("Raw", "EnergyExpenditure", "TotalEnergyExpenditure", "RestingMetabolicRate", "GoxLox", "DayNightActivity")))),
   conditionalPanel(condition = "input.ic_system == 'COSMED'", selectInput("plot_type", "Select quantity to plot", factor(c("Raw", "EnergyExpenditure", "TotalEnergyExpenditure", "RestingMetabolicRate", "GoxLox", "DayNightActivity", "EstimateRMRforCOSMED", "CompareHeatProductionFormulas")))),
   conditionalPanel(condition = "input.ic_system == 'Sable'", selectInput("plot_type", "Select quantity to plot", factor(c("Raw", "EnergyExpenditure", "TotalEnergyExpenditure", "RestingMetabolicRate", "GoxLox", "DayNightActivity", "Locomotion", "LocomotionBudget", "CompareHeatProductionFormulas")))),
   conditionalPanel(condition = "input.plot_type == 'Raw'", uiOutput("myr")),
   conditionalPanel(condition = "input.plot_type == 'GoxLox'", selectInput("goxlox", "GoxLox", choices = c("Glucose oxidation", "Lipid oxidation", "Fat oxidation", "Protein oxidation", "Nitrogen oxidation"))),
   hr(),
   h2("Grouping and filtering"),
   checkboxInput(inputId = "with_grouping", label = "Select group and filter by condition"),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("condition_type")),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("select_data_by")),
   checkboxInput(inputId = "with_facets", label = "Select a group as facet"),
   conditionalPanel(condition = "input.with_facets == true", uiOutput("facets_by_data_one")),
   conditionalPanel(condition = "input.with_facets == true", selectInput("orientation", "Orientation", choices = c("Horizontal", "Vertical"))),
   uiOutput("checkboxgroup_gender"),
   h2("Time averaging of measurements"),
   checkboxInput(inputId = "override_averaging", label = "Override averaging method (mean)"),
   conditionalPanel(condition = "input.override_averaging == true", selectInput("avg_method_for_statistics", "Method", choices = c("mean", "median"))),
   h2("Experimental times"),
   conditionalPanel(condition = "input.plot_type == 'Raw'", checkboxInput(inputId = "timeline", label = "Annotate day/night light cycle", value=TRUE)),
   conditionalPanel(condition = "input.plot_type != 'Raw'", checkboxInput(inputId = "timeline", label = "Annotate day/night light cycle")),
   checkboxInput(inputId = "only_full_days", label = "Only full days", value = FALSE),
   conditionalPanel(condition = "input.only_full_days == true", sliderInput(inputId = "full_days_threshold", label = "Fraction of day missing [%]", min = 0, max = 100, value = 0, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'Locomotion'", checkboxInput(inputId = "have_box_coordinates", label = "Custom cage coordinates", value = FALSE)),
   conditionalPanel(condition = "input.plot_type == 'Locomotion'", colourInput(inputId = "cage_color", label = "Cage Color", "white")),
   conditionalPanel(condition = "input.have_box_coordinates == true", h2("Cage configuration")),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "food_x_min", label = "Food hamper (x_min)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "food_x_max", label = "Food hamper (x_max)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "food_y_min", label = "Food hamper (y_min)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "food_y_max", label = "Food hamper (y_max)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", hr()),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "water_x_min", label = "Water bottle (x_min)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "water_x_max", label = "Water bottle (x_max)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "water_y_min", label = "Water bottle (y_min)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "water_y_max", label = "Water bottle (y_max)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", hr()),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "scale_x_min", label = "Scale (x_min)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "scale_x_max", label = "Scale (x_max)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "scale_y_min", label = "Scale (y_min)", min = 0, max = 100, value = 0)),
   conditionalPanel(condition = "input.have_box_coordinates == true", sliderInput(inputId = "scale_y_max", label = "Scale (y_max)", min = 0, max = 100, value = 0)),
   hr(),
   h2("Advanced options"),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", h3("Time Interval or Steady-State method to estimate RMR")),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", selectInput("rmr_method", "Method", choices = c("SS", "TI"))),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("rmr_method_frequency", "Frequency", min = 0, max = 30, value = 10)),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("rmr_method_begin", "Duration", min = 3, max = 10, value = 3)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", h4("CVs")),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_VO2", "VO2", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_VCO2", "VCO2", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_RER", "RER", min = 0, max = 100, value = 5)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_VE", "VE*", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.plot_type == 'EnergyExpenditure'", uiOutput("myp")),
   conditionalPanel(condition = "input.plot_type == 'EnergyExpenditure'", uiOutput("wmeans")),
   conditionalPanel(condition = "input.plot_type == 'EnergyExpenditure'", uiOutput("wmeans_choice")),
   conditionalPanel(condition = "input.plot_type == 'EnergyExpenditure'", uiOutput("wstats")),
   conditionalPanel(condition = "input.plot_type == 'EnergyExpenditure'", uiOutput("wmethod")),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("light_cycle", "Lightcycle", c("Day", "Night"), multiple = TRUE, selected = c("Day", "Night"))),
   conditionalPanel(condition = "input.plot_type == 'TotalEnergyExpenditure'", selectInput("light_cycle", "Lightcycle", c("Day", "Night"), multiple = TRUE, selected = c("Day", "Night"))),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("window", "Window size", min = 1, max = 30, value = 5, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("rmr_averaging", "Averaging width", min = 1, max = 30, value = 1, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("percentage_best", "Fraction best", min = 1, max = 100, value = 1, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("cvs", "Component:", choices = c("CO2", "O2"), multiple = FALSE, selected = "O2")),
   h3("Light cycle configuration"),
   checkboxInput(inputId = "override_metadata_light_cycle", label = "Override"),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("threshold_light_day", "Light threshold (Day)", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.plot_type == 'TotalEnergyExpenditure'", sliderInput("threshold_light_day", "Light threshold (Day)", min = 0, max = 100, value = 10)),
   sliderInput(inputId = "light_cycle_start", label = "Light cycle start", min = 0, max = 24, value = 6),
   sliderInput(inputId = "light_cycle_stop", label = "Light cycle stop", min = 0, max = 24, value = 18),
   colourInput(inputId = "light_cycle_day_color", label = "Color day", "#FFBF00"),
   colourInput(inputId = "light_cycle_night_color", label = "Color night", "#B2BEB5"),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", h3("Time averaging of raw data")),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("averaging", "Time averaging [min]", 0, 30, 10, step = 1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("running_average", "Moving average (k)", 0, 10, 1, step = 1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", selectInput("running_average_method", "Method", choices = c("Mean", "Max", "Median", "Sum"))), #nolint
   )),
   hr(),
   fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         h1("Data curation"),
         ),
         column(2, style = "padding: 20px;",
         actionButton("showTabDC", label = "", icon = icon("square-plus", "fa-3x")),
         ),
         column(2, style = "padding: 20px;",
         actionButton("hideTabDC", label = "", icon = icon("square-minus", "fa-3x")),
         )
      )
   ),
   tabsetPanel(id = "tabsDC", type = "hidden",
      tabPanelBody("DC",
   p("Selection of experimental times"),
   uiOutput("select_day"),
   uiOutput("select_animal"),
   conditionalPanel(condition = "input.do_select_date_range == true", uiOutput("daterange")),
   checkboxInput(inputId = "curate", label = "Trim data (start and end of measurement times)"),
   conditionalPanel(condition = "input.curate == true", sliderInput("exclusion_start", "Exclude hours from start of measurements", 0, 24, 2, step = 1)),
   conditionalPanel(condition = "input.curate == true", sliderInput("exclusion_end", "Exclude hours from end of measurements", 0, 24, 2, step = 1)),
   checkboxInput(inputId = "outliers", label = "Remove animal(s) from data set(s)"),
   conditionalPanel(condition = "input.outliers == true", uiOutput("sick")),
   checkboxInput("do_select_date_range", label = "Select dates"),
   )),
   hr(),
  fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         h1("Data export"),
         ),
         column(2, style = "padding: 20px;",
         actionButton("showTabDE", label = "", icon = icon("square-plus", "fa-3x")),
         ),
         column(2, style = "padding: 20px;",
         actionButton("hideTabDE", label = "", icon = icon("square-minus", "fa-3x")),
         )
      )
   ),
   tabsetPanel(id = "tabsDE", type = "hidden",
      tabPanelBody("DE",
   selectInput("export_format", "Format", choices = c("CalR", "Excel")),
   h2("Folder"),
   textInput("export_file_name", "File name (Leave empty for auto-generation of a file name)"),
   downloadButton("downloadData", "Download")
   )),
   hr(),
    fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         span(textOutput("message"), style = "color:red")
    ))),
    # hidden session ID to manage global user data (or hidden; if required to hide)
    span(textOutput("session_id"), style = "color: #77DD77; font-size: 12px; visibility: visible;")
)

################################################################################
# Main panel
################################################################################
main_content <- mainPanel(
   tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.3.1/jspdf.umd.min.js")),
   tags$style(HTML("
   .shiny-output-error {
   color: #ff6347;
   font-weight: bold;
   }
   "
   )),
   tabsetPanel(
      id = "additional_content",
      tabPanel("Basic plot", plotlyOutput("plot")),
      tabPanel("Statistical testing", uiOutput("test")),
      tabPanel("Summary statistics", plotlyOutput("summary")),
      tabPanel("Details", uiOutput("details")),
      tabPanel("Explanation", htmlOutput("explanation"))
   )
)

################################################################################
# Plotting validation
################################################################################
main_content2 <- mainPanel(
   plotOutput("plotvalidation")
)

################################################################################
# Visualization panel
################################################################################
visualization <- tabPanel(
  "Visualization and statistical analysis",
  titlePanel("Energy expenditure of cohort studies using indirect calorimetry"),
  sidebarLayout(
    sidebar_content, main_content
  )
)

################################################################################
# Validation panel
################################################################################
validation <- tabPanel(
   "Validation of calorimetry data",
   titlePanel("Validation of results by comparing RER values (CalR and our method)"),
   p("Use the file choser dialog below to select files for RER Calr and our method"),
   sidebarLayout(
      sidebar_content2, main_content2
   )
)

################################################################################
# Validation panel
################################################################################
locomotion_panel <- tabPanel(
   "Analyze locomotion",
   titlePanel("Analyze locomotion of Sable System experiments")
)

################################################################################
# Documentation
################################################################################
documentation <- tabPanel(
      "Getting help",
      style = "text-align: center",
      titlePanel("Getting general help for CALOR:"),
      div(style = "width: 50%",
      helpText("", style = "text-align: right; padding-right: 20px") %>% helper(
         type = "markdown",
         align = "right",
         content = "general_help",
         size = "l",
         colour = "red",
         style = "zoom: 300%; text-align: right"
      )
   ),
)

################################################################################
# Contact
################################################################################
contact <- tabPanel(
   "Contact",
   style = "text-align: center",
   titlePanel("Contact the author of this page:"),
   tags$address(
      p("Stephan Grein", style = "display:inline; "),
      tags$a(id = "contact_me", href = "", icon("fa-solid fa-square-envelope", "fa-1x"), style = "display:inline; "),
      br(),
      p("IRU Mathematics and Life Sciences", style = "display:inline; "),
      br(),
      p("LIMES/HCM, University of Bonn"),
      tags$script(HTML(
         "var encMail = 'c21nLmlydUBnbWFpbC5jb20K'; const form = document.getElementById('contact_me'); form.setAttribute('href', 'mailto:'.concat(atob(encMail)).concat('?subject=CALOR Shiny app'));"
      ))
   ),
   h2("Follow us on:"),
   tags$table(class = "contact", style = "margin-left: auto; margin-right: auto",
      tags$tr(
         tags$td(tags$a(href = "http://github.com/stephanmg/CALOR", icon("fa-brands fa-square-github", "fa-3x"))),
         tags$td(tags$a(href = "http://twitter.com/smgrein", icon("fa-brands fa-square-twitter", "fa-3x")))
      )
   )
)


################################################################################
# Main navigation bar
################################################################################
ui <- tagList(
  useShinyjs(),
  # Note that this is inline CSS and HTML code, can also be done as in code.js and style.css in external files
  tags$head(
   tags$style(HTML("
   .logo-container {
   position: absolute; top: 5px; left: 10px; z-index: 2147483647;
   }
   .navbar {
     padding-left: 30px;
   }
   "
   ))
  ),
  div(class="logo-container", img(src="shiny_logo.png", height="30px")),
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  navbarPage(
    header = list(use_cicerone()),
    theme = shinytheme("superhero"),
    "CALOR - A reactive web-based application for the analysis of indirect calorimetry experiments",
    intro_panel,
    visualization,
    documentation,
    contact
  )
)
