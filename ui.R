library("shinyFiles")
library("plotly")
library("shinybusy")
library("shinythemes")
library("shinyWidgets")

################################################################################
# First page: Introduction
################################################################################
intro_panel <- tabPanel(
  "Introduction",
  titlePanel("Indirect calorimetry"),
  tags$img(src="splash.jpg", align="right"),
  p("This is an R Shiny app for indirect calorimetric analysis provided TSE-CaloSys or Sable system input data"),
  br(),
  h1("Indirect calorimetry data analysis"),
  p("In this document the analysis of data acquired with TSE-CaloSys or Sable is described for"),
   tags$ul(
   tags$li("long term observations over multiple hours"),
   tags$li("short therm/ acute response experiments < 2 hours")
   ),
   h2("Features"),
   h3("Long term observational studies"),
   p("On a single data set. This description deals with an animal experiment investigating mice of two different genotypes (ko & wt) running in two cohorts (C1 & C2) and thus at two different timepoints. The provided commands are compatible with csv-files exported by the LabMaster software. Just export the respective csv-file and save it to a appropriate location."),
   h3("Removing outliers, sick animals etc."),
   p("Although not recommended you might want to exclude animals from the data. Maybe animals got sick, did not eat or technical issues with the cage occurred. One solution to remove whole animals is provided below."),
   h3("Time averaging for a more robust calorimetry analysis"),
   p("As calorimetry data is quite unstable, it is usually averaged over a certain time period (e.g. 1h as in CalR). However, one might to adjust this time period to personal preferences e.g. 30 minute intervals. This script offers this possibility. In order to do so, one must again extract some additional information form the data. In a first step, the cumulative running time of the experiment is calculated. The cumulative time (in s) is subsequently transformed in hours."),
   h3("Combining several data sets"),
   p("Limited capacity is often an issue for calorimetry, thus mice might be measured at different time points and need to be combined to a single data set. This is a issue since merging by time is not that easy due to slightly different starting time points etc. Be sure to always adjust your time period - the date can/should differ, however your starting period should always be in the same hour (e.g. 12:02:00 for the first data set and 12:00:00 for the second data set)"),
   h1("Visualization and plotting of data"),
   p("Head over to the Visualization tab in the navigation bar at the top of this window - currently single data files are supported for analysis"),
   h1("Data export"),
   p("Use the navigation bar to jump to Data export for e.g. CalR"),
   h1("Further information"),
   p("Use the navigation bar to jump Contact, About or Help for this R Shiny app. Feel free to contact SG in case of any questions")
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
   numericInput("nFiles", "Number of files", value=1, min=1, step=1),
   uiOutput("fileInputs"),
)
sidebar_content_test <- sidebarPanel(
   fluidPage(
   fluidRow(
      column(8, style="padding: 0px;",
      h1("Heat Production")),
   column(2, style="padding: 20px;",
    actionButton("showTabHP", label="", icon=icon("square-plus", "fa-3x"))
   ),
   column(2, style="padding: 20px;",
    actionButton("hideTabHP", label="", icon=icon("square-minus", "fa-3x"))
   ))),
   tabsetPanel(id="tabsHP", type="hidden",
      tabPanelBody("HP",
   add_busy_bar(color = "#FF0000"),
   withMathJax(),
   div("Chose two of the established equations for calculating the heat production in mW. Note that HP and HP2 refer to the Heldmaier equations reported in Journal of Comparative Physiology 1975, 102:115-122:"),
   selectInput("variable1", "Select first equation", choices=c("HP", "HP2", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   selectInput("variable2", "Select second equation", choices=c("HP2", "HP", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   selectInput("kj_or_kcal", "Unit of energy", choices=c("kcal", "kJ")),
   withMathJax(),
   uiOutput('heat_production_equations'),
   checkboxInput(inputId="havemetadata", label="Have metadata?"),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("metadatafile")),
   numericInput("nFiles", "Number of data files", value=1, min=1, step=1),
   uiOutput("fileInputs"),
   h3("Plotting control"),
   actionButton("plotting", "Show"),
   actionButton("reset", "Reset"),
   )),
   hr(),
   fluidPage(
   fluidRow(
      column(8, style="padding: 0px;",
      h1("Plot configuration")),
   column(2, style="padding: 20px;",
    actionButton("showTabPC", label="", icon=icon("square-plus", "fa-3x")),
   ),
   column(2, style="padding: 20px;",
    actionButton("hideTabPC", label="", icon=icon("square-minus", "fa-3x")),
   ))),
   tabsetPanel(id="tabsPC", type="hidden",
      tabPanelBody("PC",
   ### TODO: based on plot_type, add more conditional panels, so can get rid of
   ### the above sections with heat production which is confusing, so display in 
   ### case of line plot = scatterplot configurations from above, in case of 
   ### caloric equivalent the establsiehd and for box plot nothing at the moment, 
   ### these are the 2 scenarios, compare formulas for heat production (2 formulas) 
   ### and calculate heat production over time in the following then
   # Disable ANCOVA and StackedBarPlotForRMR as not yet implemented (TODO)
   selectInput("plot_type", "Type:", c("CompareHeatProductionFormulas", "CaloricEquivalentOverTime", "DayNightActivity", "Histogram", "RAW", "TotalOverDay", "RestingMetabolicRate")),
   checkboxInput(inputId="with_grouping", label="Select group and filter by condition"),
   #conditionalPanel(condition = "input.with_grouping == true", uiOutput("condition_type")),
   conditionalPanel(condition = "input.with_grouping == true", selectInput("condition_type", "Group", choices=c("Diet"))),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("select_data_by")),
   #selectInput("group_data_by", "Choose group", choices = list("Diet" = c("HDF 0.00", "CD", "HDF 0.05"), "Weight" = c("low", "high")))),
   h2("Advanced options"),
   checkboxInput(inputId="with_facets", label="Select a group as facet"),
   # TODO selectInput below needs to be replaced with UI output, to populate automatically selectInput choices from metadata..
   conditionalPanel(condition = "input.with_facets == true", uiOutput("facets_by_data_one")),
   conditionalPanel(condition = "input.with_facets == true", selectInput("orientation", "Orientation", choices=c("Horizontal", "Vertical"))),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("myp")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("wmeans")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("wstats")),
   conditionalPanel(condition = "input.plot_type == 'ANCOVA'", uiOutput("covariates")),
   conditionalPanel(condition = "input.plot_type == 'RAW'", uiOutput("myr")),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("checkboxgroup_gender")),
   # TODO: Need input percentage of lowest RMR
   # TODO: Need input number of points in interval (can be inferred automatically - > no user input, but use averaging default of 10 minutes for instance)
   # --> note could be also a user input in principal as a number (integer)
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("window", "Window", 2, 30, 10, step=1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("cvs", "Component:", choices=c("CO2", "O2"), multiple=TRUE)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("averaging", "Time averaging [min]", 1, 30, 10, step=1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("running_average", "Moving average (k)", 0, 10, 1, step=1)),
   conditionalPanel(condition = "input.plot.type != 'RestingMetabolicRate'", selectInput("running_average_method", "Method", choices=c("Max", "Mean", "Median", "Sum"))),
   )),
   hr(),
   fluidPage(
      fluidRow(
         column(8, style="padding: 0px;",
         h1("Data curation"),
         ),
         column(2, style="padding: 20px;",
         actionButton("showTabDC", label="", icon=icon("square-plus", "fa-3x")),
         ),
         column(2, style="padding: 20px;",
         actionButton("hideTabDC", label="", icon=icon("square-minus", "fa-3x")),
         )
      )
   ),
   tabsetPanel(id="tabsDC", type="hidden",
      tabPanelBody("DC",
   p("Selection of dates"),
   dateRangeInput("daterange", "Date", start="1970-01-01", end=Sys.Date()),
   sliderInput("exclusion", "Exclude hours from start of measurements", 0, 24, 2, step=1),
   sliderInput("exclusion_start", "Exclude hours from end of measurements", 0, 24, 2, step=1),
   checkboxInput(inputId="outliers", label="Remove outliers"),
   conditionalPanel(condition = "input.outliers == true", uiOutput("sick")),
   h3("Plotting status"),
   span(textOutput("message"), style="color:red"),
   )),
   hr(),
  fluidPage(
      fluidRow(
         column(8, style="padding: 0px;",
         h1("Data export"),
         ),
         column(2, style="padding: 20px;",
         actionButton("showTabDE", label="", icon=icon("square-plus", "fa-3x")),
         ),
         column(2, style="padding: 20px;",
         actionButton("hideTabDE", label="", icon=icon("square-minus", "fa-3x")),
         )
      )
   ),
   tabsetPanel(id="tabsDE", type="hidden",
      tabPanelBody("DE",
   selectInput("export_format", "Format", choices=c("CalR", "Sable", "XLSX")),
   h2("Folder"),
   textInput("export_file_name", "File name (Otherwise autogenerated)"),
   downloadButton('downloadData', 'Download')
   #shinyDirButton("export_folder", "Select a folder", "Please select a folder", FALSE),
   #h2("Folder name"),
   #uiOutput("folder_name_export"),
   #actionButton("export", "Export")
   ))
)

sidebar_content <- sidebarPanel(
   add_busy_bar(color = "#FF0000"),
   withMathJax(),
   h1("Heat production"),
   div("Chose two of the established equations for calculating the heat production in mW. Note that HP and HP2 refer to the Heldmaier equations reported in Journal of Comparative Physiology 1975, 102:115-122:"),
   selectInput("variable1", "Select first equation", choices=c("HP", "HP2", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   selectInput("variable2", "Select second equation", choices=c("HP2", "HP", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   selectInput("kj_or_kcal", "Unit of energy", choices=c("kcal", "kJ")),
   withMathJax(),
   uiOutput('heat_production_equations'),
   checkboxInput(inputId="havemetadata", label="Have metadata?"),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("metadatafile")),
   numericInput("nFiles", "Number of data files", value=1, min=1, step=1),
   uiOutput("fileInputs"),
   br(),
   h1("Plotting control"),
   actionButton("plotting", "Show"),
   actionButton("reset", "Reset"),
   h2("Plot configuration"),
   ### TODO: based on plot_type, add more conditional panels, so can get rid of
   ### the above sections with heat production which is confusing, so display in 
   ### case of line plot = scatterplot configurations from above, in case of 
   ### caloric equivalent the establsiehd and for box plot nothing at the moment, 
   ### these are the 2 scenarios, compare formulas for heat production (2 formulas) 
   ### and calculate heat production over time in the following then
   # Disable ANCOVA and StackedBarPlotForRMR as not yet implemented (TODO)
   selectInput("plot_type", "Type:", c("CompareHeatProductionFormulas", "CaloricEquivalentOverTime", "DayNightActivity", "Histogram", "RAW", "TotalOverDay", "RestingMetabolicRate")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("myp")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("wmeans")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("wstats")),
   conditionalPanel(condition = "input.plot_type == 'ANCOVA'", uiOutput("covariates")),
   conditionalPanel(condition = "input.plot_type == 'RAW'", uiOutput("myr")),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("checkboxgroup_gender")),
   # TODO: Need input percentage of lowest RMR
   # TODO: Need input number of points in interval (can be inferred automatically - > no user input, but use averaging default of 10 minutes for instance)
   # --> note could be also a user input in principal as a number (integer)
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("window", "Window", 2, 30, 10, step=1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("cvs", "Component:", choices=c("CO2", "O2"), multiple=TRUE)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("averaging", "Time averaging [min]", 1, 30, 10, step=1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("running_average", "Moving average (k)", 0, 10, 1, step=1)),
   conditionalPanel(condition = "input.plot.type != 'RestingMetabolicRate'", selectInput("running_average_method", "Method", choices=c("Max", "Mean", "Median", "Sum"))),
   h1("Data curation"),
   p("Selection of dates"),
   dateRangeInput("daterange", "Date", start="1970-01-01", end=Sys.Date()),
   sliderInput("exclusion", "Exclude hours from start of measurements", 0, 24, 2, step=1),
   sliderInput("exclusion_start", "Exclude hours from end of measurements", 0, 24, 2, step=1),
   checkboxInput(inputId="outliers", label="Remove outliers"),
   conditionalPanel(condition = "input.outliers == true", uiOutput("sick")),
   h3("Plotting status"),
   span(textOutput("message"), style="color:red"),
   h1("Data export"),
   selectInput("export_format", "Format", choices=c("CalR", "Sable", "XLSX")),
   h2("Folder"),
   textInput("export_file_name", "File name (Otherwise autogenerated)"),
   downloadButton('downloadData', 'Download')
   #shinyDirButton("export_folder", "Select a folder", "Please select a folder", FALSE),
   #h2("Folder name"),
   #uiOutput("folder_name_export"),
   #actionButton("export", "Export")
)
################################################################################
# Main panel
################################################################################
main_content <- mainPanel(
   # TODO: make this collapsible
   # https://stackoverflow.com/questions/68953107/r-shiny-make-collapsible-tabsetpanel
   # https://stackoverflow.com/questions/42159804/how-to-collapse-sidebarpanel-in-shiny-app
   tabsetPanel(
      tabPanel("Plot", plotlyOutput("plot")),
      tabPanel("Summary statistics", plotlyOutput("summary")),
      tabPanel("Help", htmlOutput("explanation"))
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
  "Visualization",
  titlePanel("Energy expenditure of cohort studies using indirect calorimetry"),
  p("Use the file choser dialog below to select an individual file to analyze"),
  sidebarLayout(
    sidebar_content_test, main_content
  )
)

################################################################################
# Validation panel
################################################################################
validation <- tabPanel(
   "Validation",
   titlePanel("Validation of results by comparing RER values (CalR and our method)"),
   p("Use the file choser dialog below to select files for RER Calr and our method"),
   sidebarLayout(
      sidebar_content2, main_content2
   )
)

################################################################################
# Documentation
################################################################################
documentation <- tabPanel(
   "Documentation",
   titlePanel("Documentation"),
   tags$img(src = "overview_shiny.png")
)

################################################################################
# Contact
################################################################################
contact <- tabPanel(
   "Contact",
   titlePanel("Contact"),
   p("SG (SG <AT> UNI <MINUS> BONN <DOT> DE")
)


################################################################################
# Main navigation bar
################################################################################
ui <- navbarPage(
  theme = shinytheme("superhero"),
  "Generalized Calorimetry Analysis",
  intro_panel,
  visualization,
  validation,
  documentation,
  contact
)