# Load libraries, data -----------------------------------------------
characters <- read.csv("data/characters.csv")


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

# Page 2 - Vizualization -------------------------------------------
select_values <- colnames(characters)
select_values <- select_values[! select_values %in% c('Character', 'Class')] # remove unwanted columns

sidebar_content <- sidebarPanel(
   fileInput("File", "Analyze calorimetic data")
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

ui <- navbarPage(
  "Generalized Calorimetry Analysis",
  intro_panel,
  second_panel
)
