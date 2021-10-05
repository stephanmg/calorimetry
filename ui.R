# Page 1 - Introduction ----------------------------------------------

intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("Indirect calorimetry"),
  
  p("This is an R Shiny app for TSE-CaloSys Data Analysis"),
  br(),
  h1("TSE-CaloSys Data Analysis"),
  p("In this document the analysis of data acquired with a TSE-CaloSys system is described for"),
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
   h1("Further information"),
   p("Use the navigation bar to jump Contact, About, Help and current TODOs for this R Shiny app. Feel free to contact SG in case of any questions")
  
)

sidebar_content <- sidebarPanel(
   withMathJax(),
   h1("Heat production"),
   div("Heat production is calculated by the following formulas (Journal of comparative physiology 1975, 102:115-122):"),
   div("$$ \\tag{1} HP[mW] = VO2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"),
   div("$$ \\tag{2} HP2[mW] = (4.44 + 1.43 \\times RER) + VO2[\\frac{ml}{h}] $$"),
   numericInput("nFiles", "Number of files", value=1, min=1, step=1),
   uiOutput("fileInputs"),
   # fileInput("File", "Analyze calorimetic data"),
   br(), br(),
 #numericRangeInput(
 #   inputId = "noui1", label = "Remove outliers",
 #   value = c(100, 400)
 # ),verbatimTextOutput(outputId="res1"),
   h1("Plotting control"),
   actionButton("plotting", "Show"),
   # actionButton("replotting", "Forced Refresh"),
   h2("Feature selection"),
   selectInput("variable1", "Variable 1:", c("HP", "V4", "V1", "V12", "V16")),
   selectInput("variable2", "Variable 2:", c("HP2", "V16", "V4", "V12", "V1")),
   h2("Plot configuration"),
   selectInput("plot_type", "Type:", c("Line plot", "Box plot")),
   sliderInput("averaging", "Time averaging [min]", 0, 120, 30),
   h3("Data curation"),
   uiOutput("sick")
)

main_content <- mainPanel(
  plotOutput("plot")
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
   "About",
   titlePanel("About the project"),
   p("To be populated...")
)

forth_panel <- tabPanel(
   "Help",
   titlePanel("Help and documentation"),
   p("To be populated...")
)

fifth_panel <- tabPanel(
   "Contact",
   titlePanel("Contact us"),
   p("To be populated")
)

sixth_panel <- tabPanel(
   "TODO",
   titlePanel("TODOs"),
   p("Finish averaging of time series (time windows for calorimetry)"),
   p("Adapt plotting to multiple files (Merge cohorts)")
)

ui <- navbarPage(
  "Generalized Calorimetry Analysis",
  intro_panel,
  second_panel,
  third_panel,
  forth_panel,
  fifth_panel,
  sixth_panel
)
