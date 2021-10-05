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
   h2("Visualization"),
   p("Head over to the Visualization tab in the navigation bar at the top of this window - currently single data files are supported for analysis")
   
  
)

sidebar_content <- sidebarPanel(
   withMathJax(),
   h1("Heat production"),
   div("Heat production is calculated by the following formulas (Journal of comparative physiology 1975, 102:115-122):"),
   div("$$ \\tag{1} HP[mW] = VO2[\\frac{ml}{h}] \\times (6 + RER + 15.3) \\times 0.278) $$"),
   div("$$ \\tag{2} HP2[mW] = (4.44 + 1.43 \\times RER + VO2[\\frac{ml}{h}] $$"),
   fileInput("File", "Analyze calorimetic data"),
   br(), br(),
 #numericRangeInput(
 #   inputId = "noui1", label = "Remove outliers",
 #   value = c(100, 400)
 # ),verbatimTextOutput(outputId="res1"),
   h1("Plotting control"),
   actionButton("plotting", "Show"),
   actionButton("unplotting", "Hide"),
   h2("Feature selection"),
   selectInput("variable1", "Variable 1:", c("HP", "V4", "V1", "V12", "V16")),
   selectInput("variable2", "Variable 2:", c("HP2", "V16", "V4", "V12", "V1")),
   h2("Plot type"),
   selectInput("plot_type", "Type:", c("Line plot", "Box plot")),
   h3("Data curation"),
   sliderInput("averaging", "Time averaging [min]", 0, 120, 30)
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
  ),
)

third_panel <- tabPanel(
   "About"
)

forth_panel <- tabPanel(
   "Help"
)

fifth_panel <- tabPanel(
   "Contact"
)

ui <- navbarPage(
  "Generalized Calorimetry Analysis",
  intro_panel,
  second_panel,
  third_panel,
  forth_panel,
  fifth_panel,
)
