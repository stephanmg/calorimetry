library("shinyFiles")
library("plotly")

# Page 1 - Introduction ----------------------------------------------
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

sidebar_content <- sidebarPanel(
   withMathJax(),
   h1("Heat production"),
   div("One of the established formulas for calculating the heat production are the Heldmaier formulas reported in Journal of Comparative Physiology 1975, 102:115-122:"),
   div("$$ \\tag{1} HP[mW] = VO2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"),
   div("$$ \\tag{2} HP2[mW] = (4.44 + 1.43 \\times RER) + VO2[\\frac{ml}{h}] $$"),
   div("If desired, choose two different formula from the drop-down menus below for a comparison (HP are the Heldmaier formulas)"),
   selectInput("variable1", "Select formula", choices=c("HP", "HP2", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   selectInput("variable2", "Select formula", choices=c("HP2", "HP", "Lusk", "Weir", "Elia", "Brower", "Ferrannini")),
   numericInput("nFiles", "Number of files", value=1, min=1, step=1),
   uiOutput("fileInputs"),
   # fileInput("File", "Analyze calorimetic data"),
   #br(), br(),
   br(),
 #numericRangeInput(
 #   inputId = "noui1", label = "Remove outliers",
 #   value = c(100, 400)
 # ),verbatimTextOutput(outputId="res1"),
   h1("Plotting control"),
   actionButton("plotting", "Show"),
   actionButton("reset", "Reset"),
   #actionButton("replotting", "Forced Refresh"),
   #h2("Feature selection"),
   #selectInput("variable1", "Variable 1:", c("HP", "V4", "V1", "V12", "V16")),
   #selectInput("variable2", "Variable 2:", c("HP2", "V16", "V4", "V12", "V1")),
   h2("Plot configuration"),
   ### TODO: based on plot_type, add more conditional panels, so can get rid of the above sections with heat production which is confusing, so display in case of line plot = scatterplot configurations
   ### from above, in case of caloric equivalent the establsiehd and for box plot nothing at the moment, these are the 2 scenarios, compare formulas for heat production (2 formulars) and calculate heat 
   ### production over time...
   selectInput("plot_type", "Type:", c("CompareHeatProductionFormulas", "CaloricEquivalentOverTime", "DayNightActivity", "StackedBarPlotForRMRandNonRMR", "ANCOVA", "Histogram")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("myp")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("wmeans")),
   conditionalPanel(condition = "input.plot_type == 'CaloricEquivalentOverTime'", uiOutput("wstats")),
   conditionalPanel(condition = "input.plot_type == 'ANCOVA'", uiOutput("covariates")),
   sliderInput("averaging", "Time averaging for RER [min]", 0, 30, 10, step=10),
   sliderInput("running_average", "n", 0, 10, 1, step=1),
   h2("Data curation"),
   p("Selection of dates"),
   dateRangeInput("daterange", "Date", start="2020-01-01", end=Sys.Date()),
   sliderInput("exclusion", "Exclude hours from start of measurements", 0, 24, 2, step=1),
   checkboxInput(inputId="outliers", label="Remove outliers"),
   conditionalPanel(condition = "input.outliers == true", uiOutput("sick")),
   h3("Plotting status"),
   span(textOutput("message"), style="color:red"),
   h1("Data export"),
   selectInput("export_format", "Format", choices=c("CalR", "Sable")),
   h2("Folder"),
   shinyDirButton("export_folder", "Select a folder", "Please select a folder", FALSE),
   h2("File name"),
   textInput("export_file_name", "File"),
   h2("Folder name"),
   uiOutput("folder_name_export"),
   actionButton("export", "Export")
)

main_content <- mainPanel(
  #plotOutput("plot")
  plotlyOutput("plot")
)

main_content2 <- mainPanel(
   plotOutput("plotvalidation")
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("Long term observational studies"),
  p("Use the file choser dialog below to select an individual file to analyze"),
  sidebarLayout(
    sidebar_content, main_content
  )
)

third_panel <- tabPanel(
   "Validation",
   titlePanel("Validation of results by comparing RER values (CalR and self)"),
   p("Use the file choser dialog below to select files for RER Calr and self"),
   sidebarLayout(
      sidebar_content2, main_content2
   )
)

forth_panel <- tabPanel(
   "Help",
   titlePanel("Help and documentation"),
   p("TODO")
)

fifth_panel <- tabPanel(
   "Contact",
   titlePanel("Contact us"),
   p("TODO")
)

sixth_panel <- tabPanel(
   "Data export",
   titlePanel("Data export"),
   p("TODO")
)

ui <- navbarPage(
  "Generalized Calorimetry Analysis",
  intro_panel,
  second_panel,
  sixth_panel,
  third_panel,
  forth_panel,
  fifth_panel,
)
