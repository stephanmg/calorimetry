library("shinyFiles")
library("plotly")
library("shinybusy")
library("shinythemes")
library("shinyWidgets")
library("shinyhelper")
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
   actionButton("plottingvalidation", "Show"),
   actionButton("reset", "Reset"),
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
      h1("Main Configuration"),
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
   h3("Equations"),
   div("Chose two established equations for calculating the heat production respectively energy expenditure. Note that the abbreviations HP and HP2 refer to Heldmaier's equations as reported in the publication J Comp Physiol B 102, 115–122 (1975)."),
   selectInput("variable1", "Select first equation", choices = c("HP", "HP2", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   selectInput("variable2", "Select second equation", choices = c("HP2", "HP", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   selectInput("kj_or_kcal", "Unit of energy", choices = c("kJ", "kcal")),
   withMathJax(),
   uiOutput("heat_production_equations"),
   h3("Metadata"),
   div("Additional metadata might be provided through upload of a standardized, using the controlled vocabulary, filled-in Excel metadata sheet for the (whole) experiment. Check the box below if you wish to provide some complementing metadata for analysis."),
   checkboxInput(inputId = "havemetadata", label = "Have additional metadata?"),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("metadatafile")),
   h3("Data sets"),
   p("Use the file choser dialog below to select an individual file to analyze"),
   numericInput("nFiles", "Number of data files", value = 1, min = 1, step = 1),
   uiOutput("fileInputs"),
   h3("Data consistency checks"),
   div("In case of any detected inconsistency in the raw data, a warning is generated, and further analysis is postponed if boxes are checked."),
   checkboxInput(inputId = "negative_values", label = "Detect negative values", value = FALSE),
   checkboxInput(inputId = "highly_varying_measurements", label = "High variation measurements", value = FALSE),
   h3("Plotting control"),
   actionButton("plotting", "Show"),
   actionButton("reset", "Reset"),
   span(textOutput("file_type_detected"), style = "color:green; font-weight: bold;"),
   span(textOutput("study_description"), style = "color:orange; font-weight: bold;"),
   ))),
   hr(),
   fluidPage(
   fluidRow(
      column(8, style = "padding: 0px;",
      h1("Plot configuration")),
   column(2, style = "padding: 20px;",
    actionButton("showTabPC", label = "", icon = icon("square-plus", "fa-3x")),
   ),
   column(2, style = "padding: 20px;",
    actionButton("hideTabPC", label = "", icon = icon("square-minus", "fa-3x")),
   ))),
   tabsetPanel(id = "tabsPC", type = "hidden",
      tabPanelBody("PC",
   div("A general workflow: First inspect raw data, then calculate energy expenditures and compare TEE and RMR. If locomotion data available plot the budget and probability density maps. Export compiled data sets to Excel or CalR."),
   selectInput("plot_type", "Type:", factor(c("Raw", "EnergyExpenditure", "TotalEnergyExpenditure", "RestingMetabolicRate", "GoxLox", "DayNightActivity", "Locomotion", "LocomotionBudget", "WeightVsEnergyExpenditure", "EstimateRMRforCOSMED"))),
   hr(),
   h2("Grouping and filtering"),
   checkboxInput(inputId = "with_grouping", label = "Select group and filter by condition"),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("condition_type")),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("select_data_by")),
   checkboxInput(inputId = "with_facets", label = "Select a group as facet"),
   conditionalPanel(condition = "input.with_facets == true", uiOutput("facets_by_data_one")),
   conditionalPanel(condition = "input.with_facets == true", selectInput("orientation", "Orientation", choices = c("Horizontal", "Vertical"))),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("checkboxgroup_gender")),
   h2("Experimental times"),
   checkboxInput(inputId = "timeline", label = "Annotate day/night light cycle"),
   checkboxInput(inputId = "day_only", label = "Day", value = TRUE),
   checkboxInput(inputId = "night_only", label = "Night", value = TRUE),
   conditionalPanel(condition = "input.plot_type == 'WeightVsEnergyExpenditure'", selectInput("statistics", "Statistics", choices = c("mean", "median", "mean_sdl"))),
   conditionalPanel(condition = "input.plot_type == 'TotalEnergyExpenditure'", checkboxInput(inputId = "only_full_days", label = "Only full days", value = TRUE)),
   conditionalPanel(condition = "input.plot_type == 'Locomotion'", checkboxInput(inputId = "have_box_coordinates", label = "Custom cage coordinates", value = FALSE)),
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
   conditionalPanel(condition = "input.plot_type == 'Raw'", uiOutput("myr")),
   conditionalPanel(condition = "input.plot_type == 'GoxLox'", selectInput("goxlox", "GoxLox", choices = c("Glucose oxidation", "Lipid oxidation", "Fat oxidation", "Protein oxidation", "Nitrogen oxidation"))),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("window", "Window", 2, 30, 10, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("cvs", "Component:", choices = c("CO2", "O2"), multiple = TRUE)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("light_cycle", "Lightcycle", c("Day", "Night"))),
   h3("Light cycle configuration"),
   checkboxInput(inputId = "override_metadata_light_cycle", label = "Override"),
   sliderInput(inputId = "light_cycle_start", label = "Light cycle start", min = 0, max = 24, value = 7),
   sliderInput(inputId = "light_cycle_stop", label = "Light cycle stop", min = 0, max = 24, value = 19),
   colourInput(inputId = "light_cycle_day_color", label = "Color day", "#FFBF00"),
   colourInput(inputId = "light_cycle_night_color", label = "Color night", "#B2BEB5"),
   h3("Time averaging of raw data"),
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
   p("Selection of dates"),
   dateRangeInput("daterange", "Date", start = "1970-01-01", end = Sys.Date()),
   sliderInput("exclusion", "Exclude hours from start of measurements", 0, 24, 2, step = 1),
   sliderInput("exclusion_start", "Exclude hours from end of measurements", 0, 24, 2, step = 1),
   checkboxInput(inputId = "outliers", label = "Remove outliers"),
   conditionalPanel(condition = "input.outliers == true", uiOutput("sick")),
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
   textInput("export_file_name", "File name (Leave empty for auto-generation)"),
   downloadButton("downloadData", "Download")
   )),
   hr(),
    fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         span(textOutput("message"), style = "color:red")
    )))
)

################################################################################
# Main panel
################################################################################
main_content <- mainPanel(
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
