################################################################################
# Required libraries
################################################################################
library(shinyFiles)
library(plotly)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(shinyhelper)
library(shinyjs)
library(colourpicker)
library(cicerone)

################################################################################
# Introduction and Features panel (Landing page of the CALOR web application)
################################################################################
intro_panel <- tabPanel(
  "Home",
  #titlePanel("A reactive web-based application for the analysis of indirect calorimetry experiments"),
  div(class="tile_upper_landing_page",
  div(img(src = "shiny_logo.png", height="400px"), style = "text-align: center; display: flex; justify-content: center; align-items: center;"),
  br(),
  p("A Shiny/R web application for general indirect calorimetry analysis", style="text-align: center; display: flex; justify-content: center; align-items: center;"),
  div(actionButton("btn_to_analysis", "Go to analysis", class="custom-button"), style = "text-align: center; display: flex; justify-content: center; align-items: center;"),
  ),
  div(class="tile_lower_landing_page",
  h2("Visualization and statistical data analysis", style="text-align:center;"),
  div(class="image-container", tags$a(href="https://calorimetry.readthedocs.io/en/latest/introduction.html", target="_blank", img(src = "app_landing_page.png", class="zoom-image")), style = "text-align: center; display: flex; justify-content: center; align-items: center;"),
  ),
  br(),
  div(class="tile_bottom_landing_page",
  h2("Metadata analysis", style="text-align:center;"),
  div(class="image_container", tags$a(href="https://calorimetry.readthedocs.io/en/latest/metadata.html", target="_blank", tags$img(src = "app_landing_page_bottom.png", class="zoom-image")), style = "text-align: center; display: flex; justify-content: center; align-items: center;"),
  ),
  #p("A R Shiny web app for the analysis of indirect calorimetric data provided in standardized data formats from the TSE Phenomaster/Phenolab, Sable Promethion and Columbus Instruments CLAMS system."), #nolint
  #p("In this document the analysis of data acquired during indirect calorimetry experiments is explained for: "),
  # tags$ul(
  # tags$li("long term observations over multiple hours"),
  # tags$li("short therm/acute response experiments < 2 hours")
  # ),
  div(class="flex-container",
  div(class="flex-div-right",
    span("CALOR - A web application for general indirect calorimetry", style = "font-size: 12px; visibility: visible;"),
    tags$a(id = "contact_me", href = "", icon("fa-solid fa-square-envelope", "fa-1x"), style = "display:inline; "),
    tags$a(href = "http://github.com/stephanmg/CALOR", icon("fa-brands fa-square-github", "fa-1x")),
    tags$a(href = "http://twitter.com/smgrein", icon("fa-brands fa-square-x-twitter", "fa-1x")),
    tags$a(href = "http://youtube.com/@CALOR-APP", icon("fa-brands fa-square-youtube", "fa-1x"))
   ))
)

################################################################################
# validation sidebar panel
################################################################################
validation_panel <- sidebarPanel(
   fileInput("rerself", "RER self (means)"),
   fileInput("rercalr", "RER CalR (means)"),
   h1("Plotting control"),
   textInput("plotTitle", "Plot title", paste0("Average of RER value")),
   actionButton("plottingvalidation", "Show plots"),
   actionButton("reset", "Reset session"),
)

################################################################################
# file loading sidebar panel
################################################################################
sidebar_file_panel <- sidebarPanel(
   numericInput("nFiles", "Number of files", value = 1, min = 1, step = 1),
   uiOutput("fileInputs"),
)


################################################################################
# sidebar content
################################################################################
sidebar_content <- sidebarPanel(
   fluidPage(
   fluidRow(
      column(8, style = "padding: 0px;",
      h3("Dataset import"),
      br(),
      h4("Examples"),
      actionButton("example_data_single", "UCP1 KO", style = "border: 1px solid white; background-color: rgba(42,82,190,0.5)"),
      actionButton("example_data_single_alternative", "DAKO", style = "border: 1px solid white; background-color: rgba(213,173,65,0.5)"),
      actionButton("guide", "User guide", style = "border: 1px solid white; background-color: rgba(255,69,0,0.5)"),
      hr(width="150%"),
   ),
   column(2, style = "padding: 20px;",
    actionButton("showTabHP", label = "", icon = icon("square-plus", "fa-3x"))
   ),
   column(2, style = "padding: 20px;",
    actionButton("hideTabHP", label = "", icon = icon("square-minus", "fa-3x"))
   )),
   tabsetPanel(id = "tabsHP", type = "hidden",
      tabPanelBody("HP",
   add_busy_bar(color = "#0FFF50"),
   withMathJax(),
   h3("Energy expenditure equation"),
   conditionalPanel("input.plot_type != 'CompareHeatProductionFormulas'", selectInput("variable1", "Select equation", choices = c("Heldmaier1", "Heldmaier2", "Weir", "Ferrannini"), selected="Heldmaier2")),
   conditionalPanel("input.plot_type == 'CompareHeatProductionFormulas'", selectInput("variable2", "Select second equation", choices = c("Heldmaier1", "Heldmaier2", "Lusk", "Weir", "Elia", "Brouwer", "Ferrannini"))),
   selectInput("kj_or_kcal", "Unit of energy", choices = c("kJ", "kcal", "mW")),
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
   checkboxInput(inputId = "havemetadata", label = "Provide metadata"),
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
   checkboxInput(inputId="use_zeitgeber_time", "Use zeitgeber time", value = TRUE),
   checkboxInput(inputId="recalculate_RER", "Re-calculate RER", value = TRUE),
   conditionalPanel(condition = "input.coarsen_data_sets == true", numericInput("coarsening_factor", "Factor", value = 1, min = 1, max = 10, step=1)),
   checkboxInput(inputId="use_raw_data_curation", "RawMeasurement data curation", value = FALSE),
   conditionalPanel(condition  ="input.use_raw_data_curation == true", 
      h3("RawMeasurement data curation"),
      checkboxInput(inputId = "z_score_removal_of_outliers", label = "Remove outliers automatically by z-score"),
      conditionalPanel(condition = "input.z_score_removal_of_outliers == true", numericInput("sds", "Number of SDs", value = 2, step=1, min = 0, max = 3)),
      conditionalPanel(condition = "input.z_score_removal_of_outliers == true", selectInput("target_columns", "Measurements", c("VO2", "VCO2"), multiple=TRUE, selected=c("VO2", "VCO2"))),
      checkboxInput(inputId = "remove_zero_values", label = "Remove zero values automatically"),
      conditionalPanel(condition = "input.remove_zero_values == true", numericInput("eps", "Epsilon", value=1e-6, min=1e-9, max=1e-3, step=1e-3)),
      checkboxInput(inputId = "toggle_outliers", "Manually mark outliers above threshold", value = FALSE),
      conditionalPanel("input.toggle_outliers == true", numericInput(inputId = "threshold_toggle_outliers", "Threshold", value=20.67, min = 0, max = 100, step = 0.01)),
      checkboxInput(inputId = "toggle_lasso_outliers", "Select and remove outliers by box selection"), 
      conditionalPanel("input.toggle_lasso_outliers == true", actionButton("remove_lasso_points", "Remove lasso selection")),
      h3("RawMeasurement data consistency checks"),
      checkboxInput(inputId = "negative_values", label = "Detect negative values", value = FALSE),
      checkboxInput(inputId = "detect_nonconstant_measurement_intervals", label = "Detect non-constant measurement intervals", value = FALSE),
      checkboxInput(inputId = "highly_varying_measurements", label = "Detect highly varying measurements", value = FALSE),
      conditionalPanel("input.highly_varying_measurements == true", sliderInput("threshold_for_highly_varying_measurements", "Threshold [%]", min = 0, max = 200, step = 10, value = 200)),
   ),
   hr()
   ))),

   fluidPage(
   fluidRow(
      column(8, style = "padding: 0px;",
      h3("Visualization of data")),

   column(2, style = "padding: 20px;",
    actionButton("showTabPC", label = "", icon = icon("square-plus", "fa-3x")),
   ),
   column(2, style = "padding: 20px;",
    actionButton("hideTabPC", label = "", icon = icon("square-minus", "fa-3x")),
   ))),
   tabsetPanel(id = "tabsPC", type = "hidden",
      tabPanelBody("PC",
   selectInput(inputId = "ic_system", "Select indirect calorimetry platform", factor(c("General", "COSMED", "Sable"))),
   conditionalPanel(condition = "input.ic_system == 'General'", selectInput("plot_type", "Select quantity to plot", factor(c("Metadata", "RawMeasurement", "TotalHeatProduction", "RestingMetabolicRate", "HeatProduction", "FuelOxidation")))),
   conditionalPanel(condition = "input.ic_system == 'COSMED'", selectInput("plot_type", "Select quantity to plot", factor(c("Metadata", "RawMeasurement", "TotalHeatProduction", "RestingMetabolicRate", "HeatProduction", "FuelOxidation", "EstimateRMRforCOSMED", "CompareHeatProductionFormulas")))),
   conditionalPanel(condition = "input.ic_system == 'Sable'", selectInput("plot_type", "Select quantity to plot", factor(c("Metadata", "RawMeasurement", "TotalHeatProduction", "RestingMetabolicRate", "HeatProduction", "FuelOxidation", "Locomotion", "LocomotionBudget", "CompareHeatProductionFormulas")))),
   conditionalPanel(condition = "input.plot_type == 'RawMeasurement'", uiOutput("myr")),
   conditionalPanel(condition = "input.plot_type == 'FuelOxidation'", selectInput("goxlox", "FuelOxidation", choices = c("Glucose oxidation", "Lipid oxidation", "Fat oxidation", "Protein oxidation", "Nitrogen oxidation"))),
   conditionalPanel(condition = "input.plot_type == 'TotalHeatProduction' || input.plot_type == 'DayNightActivity'", selectInput("box_violin_or_other", "Type of visualization", c("Boxplot", "Violinplot", "Dotplot"), selected="Violinplot")),
   conditionalPanel(condition = "input.plot_type == 'DayNightActivity'", selectInput("box_violin_or_other", "Type of visualization", c("Boxplot", "Violinplot", "Dotplot"), selected="Boxplot")),
   hr(),
   h2("Grouping and filtering"),
   checkboxInput(inputId = "with_grouping", label = "Select group and filter by condition"),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("condition_type")),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("select_data_by")),
   checkboxInput(inputId = "with_facets", label = "Select a group as facet"),
   conditionalPanel(condition = "input.with_facets == true", uiOutput("facets_by_data_one")),
   conditionalPanel(condition = "input.with_facets == true", selectInput("orientation", "Orientation", choices = c("Horizontal", "Vertical"))),
   checkboxInput(inputId = "select_temperature", label = "Select temperature"),
   conditionalPanel(condition = "input.select_temperature == true", sliderInput(inputId = "temperature_mean", label = "Temperature", min=0, max=30, value=4, step=1)),
   conditionalPanel(condition = "input.select_temperature == true", sliderInput(inputId = "temperature_deviation", label = "Deviation from temperature", min=0, max=1, value=0.05, step=0.05)),
   uiOutput("checkboxgroup_gender"),
     h2("Experimental times"),
   checkboxInput(inputId = "timeline", label = "Annotate day/night light cycle"),
   conditionalPanel(condition = "input.use_zeitgeber_time != false",
   checkboxInput(inputId = "only_full_days_zeitgeber", label = "Select full days based on zeitgeber time", value = FALSE)),
   conditionalPanel(condition = "input.use_zeitgeber_time == false",
   checkboxInput(inputId = "only_full_days", label = "Select full consecutive calendrical days", value = FALSE)),
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
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("myp")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wmeans")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wmeans_choice")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wstats")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wmethod")),
   selectInput("light_cycle", "Lightcycle", c("Day", "Night"), multiple = TRUE, selected = c("Day", "Night")),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("window", "Window size", min = 1, max = 30, value = 5, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("rmr_averaging", "Averaging width", min = 1, max = 30, value = 1, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("percentage_best", "Fraction best", min = 1, max = 100, value = 1, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("cvs", "Component:", choices = c("CO2", "O2"), multiple = FALSE, selected = "O2")),
   h3("Time averaging of measurements"),
   checkboxInput(inputId = "override_averaging", label = "Override averaging method (mean)"),
   conditionalPanel(condition = "input.override_averaging == true", selectInput("avg_method_for_statistics", "Method", choices = c("mean", "median"))),
   h3("Light cycle configuration"),
   checkboxInput(inputId = "override_metadata_light_cycle", label = "Override"),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("threshold_light_day", "Light threshold (Day)", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.plot_type == 'TotalHeatProduction'", sliderInput("threshold_light_day", "Light threshold (Day)", min = 0, max = 100, value = 10)),
   sliderInput(inputId = "light_cycle_start", label = "Light cycle start", min = 0, max = 24, value = 6),
   sliderInput(inputId = "light_cycle_stop", label = "Light cycle stop", min = 0, max = 24, value = 18),
   colourInput(inputId = "light_cycle_day_color", label = "Color day", "#FFBF00"),
   colourInput(inputId = "light_cycle_night_color", label = "Color night", "#B2BEB5"),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", h3("Time averaging of raw data")),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("averaging", "Time averaging [min]", 0, 30, 10, step = 1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("running_average", "Moving average (k)", 0, 10, 1, step = 1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", selectInput("running_average_method", "Method", choices = c("Mean", "Max", "Median", "Sum"))), #nolint
   )),
   h4("Controls"),
   tags$style(HTML("
        #reset {
          background-color: #637DFF; /* Pastel Blue */
          color: white;
          //border-color: #637DFF;
          border-color: white;
        }
        #reset:hover {
          background-color: #A3B8FF;
          //border-color: #A3B8FF;
          border-color: white;
        }
        #plotting {
          background-color: #77DD77; /* Pastel Green */
          color: white;
          //border-color: #77DD77;
          border-color: white;
        }
        #plotting:hover {
          background-color: #5CB85C; 
          //border-color: #5CB85C;
          border-color: white;
        }
        #refresh {
         background-color: #8DBBD0; /* Pastel Light Blue */
         //border-color: #8DBBD0;
          border-color: white;
        }
        #refresh:hover {
         background-color: #6E98AA;
          border-color: white;
         //border-color: #6E98AA
        }
        #load_data { /* Pastel Red */
         background-color: #FF6961;
         //border-color: #FF6961;
          border-color: white;
        }
        #load_data:hover {
        background-color: #FFB3AB;
        //border-color: #FFB3AB;
          border-color: white;
        }
   ")),
   actionButton("load_data", "Load data"), br(),
   actionButton("plotting", "Show plot",), br(),
   actionButton("refresh", "Refresh"), br(),
   actionButton("reset", "Reset"), br(),
   checkboxInput("use_default_plot_style", "Use default plot style", value=TRUE),


   hr(),
   fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         h3("Data curation"),
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
   # TODO: Calendrical day selection not yet implemented, needs changes to utility function for zeitgeber time, thus the do_select_date_range checkbox is disabled now
   conditionalPanel(condition = "input.use_zeitgeber_time == false", checkboxInput("do_select_date_range", label = "Select dates", value=FALSE)),
   tags$script(HTML("$('#do_select_date_range').prop('disabled', true);")),
   tags$script(HTML("$('#use_default_plot_style').prop('disabled', true);"))
   )),
   hr(),
  fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         h3("Result summary"),
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
   h3("Selected and calculated quantities", title = "This will export the currently selected quantity (see plotting above) and all quantities calculated during calculations"),
   selectInput("export_format", "Format", choices = c("CalR", "Excel")),
   textInput("export_file_name", "File name (Leave empty for auto-generation of a file name)"),
   downloadButton("downloadData", "Download"),
   h3("All data (Input and data frames for plotting)", title = "This will export all calculated data, the selected quantity for plotting, as well as all data frames so far for plotting. Note that in order to export TEE or RMR, you need to calculate both TEE and RMR first."),
   downloadButton("downloadAllData", "Download as zip file", icon=icon("file-archive")),
   h3("Data tables for plotting", title = "This will download only the data tables for plotting"),
   textInput("export_file_name2", "File name (Leave empty for auto-generation of a file name)"),
   downloadButton("downloadPlottingData", "Download"),
   )),
   #hr(),
    fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         span(textOutput("message"), style = "color:red")
    ))),
  )

################################################################################
# main content panel
################################################################################
main_content <- mainPanel(
   width=8,
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
      tabPanel("Main plot", 
         tagList(
            conditionalPanel("input.plot_type != 'Metadata'", h3("Overview")),
            conditionalPanel("input.plot_type != 'Metadata'", checkboxInput("windowed_plot", "Add windowed time-trace plot", value=FALSE)),
            plotlyOutput("plot"),
            conditionalPanel("output.plotRendered && input.plot_type != 'Metadata'", checkboxInput("stylize_plot", "Stylize plot")),
            conditionalPanel("input.stylize_plot == true", uiOutput("stylize_plot_plotting_control")),
            conditionalPanel("output.plotRendered && input.plot_type != 'Metadata'", checkboxInput("add_average_with_se", "Smooth facets")),
            conditionalPanel("output.plotRendered && input.plot_type != 'Metadata' && input.add_average_with_se == true", checkboxInput("add_average_with_se_one_plot", "One plot", value=FALSE)),
            conditionalPanel("input.add_average_with_se == true", selectInput("averaging_method_with_facets", "Method for smoothing", choices=c("gam", "sawitzky-golay"), selected="gam")),
            conditionalPanel("input.add_average_with_se == true", numericInput("averaging_method_with_facets_confidence_levels", "SE", min=1, max=10, value=2)),
            conditionalPanel("input.add_average_with_se == true", numericInput("averaging_method_with_facets_basis_functions", "Number of Basis functions", min = 10, max=40, value=20)),
            conditionalPanel("input.add_average_with_se == true", selectInput("averaging_method_with_facets_basis_function", "Basis function", choices=c("cs", "tp", "cr", "ps", "gp", "ts"), selected="cr")),
            conditionalPanel("input.add_average_with_se == true", numericInput("averaging_method_with_facets_alpha_level", "Transparency level", min=0.0, max=1.0, value=0.2, step=0.05)),
            conditionalPanel("input.add_average_with_se == true && input.with_facets != true", colourInput("averaging_method_with_facets_color", "Color", "blue")),
            conditionalPanel("output.plotRendered && input.plot_type == 'TotalHeatProduction'", checkboxInput("add_time_trace_below", "Add time trace(s)")),
            conditionalPanel("output.plotRendered && input.plot_type == 'TotalHeatProduction' && input.add_time_trace_below == true", plotlyOutput("timeTrace")),
            conditionalPanel("input.windowed_plot == true", hr()),
            conditionalPanel("input.windowed_plot == true && input.plot_type != 'Metadata'", h3("Windowed time-trace plot")),
            conditionalPanel("(output.plotRendered && (input.plot_type == 'RawMeasurement' || input.plot_type == 'HeatProduction' || input.plot_type == 'FuelOxidation' || input.plot_type == 'TotalHeatProduction' || input.plot_type == 'RestingMetabolicRate')) && input.windowed_plot == true", plotlyOutput("windowPlot")),
            conditionalPanel("input.windowed_plot == true", sliderInput("interval_length_for_window", "Interval length", min=5, max=240, value=30)),
            conditionalPanel("input.windowed_plot == true", sliderInput("interval_steps_for_window", "Steps", min=1, max=10, value=2)),
            conditionalPanel("input.windowed_plot == true", checkboxInput("boxplots_or_sem_plots", "Time boxplot", value=FALSE)),
            conditionalPanel("input.windowed_plot == true && input.connect_medians_of_boxplots != true && input.with_facets == true", checkboxInput("facet_medians", "Display only facet medians", value=FALSE)),
            conditionalPanel("input.windowed_plot == true && input.connect_medians_of_boxplots != true && input.with_facets == true", checkboxInput("facet_medians_statistics", "Display test statistics", value=FALSE)),
            conditionalPanel("input.facet_medians == true", checkboxInput("facet_medians_in_one_plot", "One plot", value=TRUE)),
            conditionalPanel("input.windowed_plot == true && input.boxplots_or_sem_plots == true", checkboxInput("connect_medians_of_boxplots", "Connect individual medians", value=FALSE))
         )
      ),
      tabPanel("Statistical testing", uiOutput("test")),
      tabPanel("Summary statistics", plotlyOutput("summary")),
      tabPanel("Details", uiOutput("details")),
      tabPanel("Statistical model", uiOutput("modelling")),
      tabPanel("Explanation", htmlOutput("explanation"))
   )
)

################################################################################
# Plotting validation
################################################################################
plotting_validation_panel <- mainPanel(
   plotOutput("plotvalidation")
)

page_for_data_export <-  fluidPage(
   tabsetPanel(id = "tabsDE", type = "hidden",
      tabPanelBody("DE",
   h4("Selected and calculated quantities", title = "This will export the currently selected quantity (see plotting above) and all quantities calculated during calculations"),
   selectInput("export_format", "Format", choices = c("CalR", "Excel")),
   textInput("export_file_name", "File name (Leave empty for auto-generation of a file name)"),
   downloadButton("downloadData", "Download"),
   h4("All data (Input and data frames for plotting)", title = "This will export all calculated data, the selected quantity for plotting, as well as all data frames so far for plotting. Note that in order to export TEE or RMR, you need to calculate both TEE and RMR first."),
   downloadButton("downloadAllData", "Download as zip file", icon=icon("file-archive")),
   h4("Data tables for plotting", title = "This will download only the data tables for plotting"),
   textInput("export_file_name2", "File name (Leave empty for auto-generation of a file name)"),
   downloadButton("downloadPlottingData", "Download"),
   )),
   #hr(),
    fluidPage(
      fluidRow(
         column(8, style = "padding: 0px;",
         span(textOutput("message"), style = "color:red")
    ))))

page_for_data_curation <- fluidPage(
   h4("Trimming"),
   tabsetPanel(id = "tabsDC", type = "hidden",
      tabPanelBody("DC",
   p("Selection of experimental times"),
   conditionalPanel(condition = "input.do_select_date_range == true", uiOutput("daterange")),
   checkboxInput(inputId = "curate", label = "Trim data (start and end of measurement times)"),
   conditionalPanel(condition = "input.curate == true", sliderInput("exclusion_start", "Exclude hours from start of measurements", 0, 24, 2, step = 1)),
   conditionalPanel(condition = "input.curate == true", sliderInput("exclusion_end", "Exclude hours from end of measurements", 0, 24, 2, step = 1)),
   # TODO: Calendrical day selection not yet implemented, needs changes to utility function for zeitgeber time, thus the do_select_date_range checkbox is disabled now
   conditionalPanel(condition = "input.use_zeitgeber_time == false", checkboxInput("do_select_date_range", label = "Select dates", value=FALSE)),
   tags$script(HTML("$('#do_select_date_range').prop('disabled', true);")),
   tags$script(HTML("$('#use_default_plot_style').prop('disabled', true);"))
      )))

page_for_data_curation_selection <- fluidPage(
   h4("Select days and samples"),
   tabsetPanel(id = "tabsDC", type = "hidden",
      tabPanelBody("DC",
   uiOutput("select_day"),
   uiOutput("select_animal"),
   div(actionButton("apply_selection", "Apply selection"), style="text-align: center; padding: 5px;")
      )))



page_for_data_import_example_data <- fluidPage(
   h4("Examples"),
   div(actionButton("example_data_single", "UCP1 KO study", style = "margin: 5px; width: 200px; border: 1px solid white; background-color: rgba(42,82,190,0.5)"), style="text-align: center"), br(),
   div(actionButton("example_data_single_alternative", "DAKO study", style = "margin: 5px; width: 200px; border: 1px solid white; background-color: rgba(213,173,65,0.5)"), style="text-align: center"),
   h4("User guide"),
   div(actionButton("guide", "Getting help", style = "margin: 5px; width: 200px; border: 1px solid white; background-color: rgba(255,69,0,0.5)"), style="text-align: center"),
)

page_for_data_import_select_equation <- fluidPage(
   withMathJax(),
   h4("Heat production"),
   conditionalPanel("input.plot_type != 'CompareHeatProductionFormulas'", selectInput("variable1", "Select equation", choices = c("Heldmaier1", "Heldmaier2", "Weir", "Ferrannini"), selected="Heldmaier2")),
   conditionalPanel("input.plot_type == 'CompareHeatProductionFormulas'", selectInput("variable2", "Select second equation", choices = c("Heldmaier1", "Heldmaier2", "Lusk", "Weir", "Elia", "Brouwer", "Ferrannini"))),
   selectInput("kj_or_kcal", "Unit of energy", choices = c("kJ", "kcal", "mW")),
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
   uiOutput("heat_production_equations")
)

page_for_data_import <- fluidPage(
  # fluidRow(
  #    column(8, style = "padding: 0px;",
  #    h3("Dataset import"),
  #    br(),
  #    h4("Examples"),
  #    actionButton("example_data_single", "UCP1 KO", style = "border: 1px solid white; background-color: rgba(42,82,190,0.5)"),
  #    actionButton("example_data_single_alternative", "DAKO", style = "border: 1px solid white; background-color: rgba(213,173,65,0.5)"),
  #    actionButton("guide", "User guide", style = "border: 1px solid white; background-color: rgba(255,69,0,0.5)"),
  #    hr(width="150%"),
  #    )),
   tabsetPanel(id = "tabsHP", type = "hidden",
      tabPanelBody("HP",
   add_busy_bar(color = "#0FFF50"),
   h3("Metadata"),
   div("Provided through standardized Excel metadata sheets [7]"),
   checkboxInput(inputId = "havemetadata", label = "Have additional metadata?"),
   conditionalPanel(condition = "input.havemetadata == true", uiOutput("metadatafile")),
   h3("Datasets"),
   p("Specify number of file(s)"),
   numericInput("nFiles", "Number of cohorts", value = 1, min = 1, step = 1),
   uiOutput("fileInputs"),
   h4(textOutput("additional_information")),
   span(textOutput("file_type_detected"), style = "color:green; font-weight: bold;"),
   span(textOutput("study_description"), style = "color:orange; font-weight: bold;"),
   tags$style(HTML("
        #reset {
          background-color: #637DFF; /* Pastel Blue */
          color: white;
          //border-color: #637DFF;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #reset:hover {
          background-color: #A3B8FF;
          //border-color: #A3B8FF;
          border-color: white;
        }
        #plotting {
          background-color: #77DD77; /* Pastel Green */
          color: white;
          //border-color: #77DD77;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #plotting:hover {
          background-color: #5CB85C; 
          //border-color: #5CB85C;
          border-color: white;
        }
        #refresh {
         background-color: #8DBBD0; /* Pastel Light Blue */
         //border-color: #8DBBD0;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #refresh:hover {
         background-color: #6E98AA;
          border-color: white;
         //border-color: #6E98AA
        }
        #load_data { /* Pastel Red */
         background-color: #FF6961;
         //border-color: #FF6961;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #load_data:hover {
        background-color: #FFB3AB;
        //border-color: #FFB3AB;
          border-color: white;
        }
   ")),
   actionButton("load_data", "Load data"), br(),
   actionButton("plotting", "Show plot",), br(),

      )))

page_for_data_import_preprocessing <- fluidPage(
   h4("Preprocessing"),
   checkboxInput(inputId="coarsen_data_sets", "Coarsen datasets"),
   checkboxInput(inputId="use_zeitgeber_time", "Use zeitgeber time", value = TRUE),
   checkboxInput(inputId="recalculate_RER", "Re-calculate RER", value = TRUE),
   conditionalPanel(condition = "input.coarsen_data_sets == true", numericInput("coarsening_factor", "Factor", value = 1, min = 1, max = 10, step=1)),
   checkboxInput(inputId="use_raw_data_curation", "Amend raw data", value = FALSE),
   conditionalPanel(condition  ="input.use_raw_data_curation == true", 
      h4("Raw data curation"),
      checkboxInput(inputId = "z_score_removal_of_outliers", label = "Remove outliers automatically by z-score"),
      conditionalPanel(condition = "input.z_score_removal_of_outliers == true", numericInput("sds", "Number of SDs", value = 2, step=1, min = 0, max = 3)),
      conditionalPanel(condition = "input.z_score_removal_of_outliers == true", selectInput("target_columns", "Measurements", c("VO2", "VCO2"), multiple=TRUE, selected=c("VO2", "VCO2"))),
      checkboxInput(inputId = "remove_zero_values", label = "Remove zero values automatically"),
      conditionalPanel(condition = "input.remove_zero_values == true", numericInput("eps", "Epsilon", value=1e-6, min=1e-9, max=1e-3, step=1e-3)),
      checkboxInput(inputId = "toggle_outliers", "Manually mark outliers above threshold", value = FALSE),
      conditionalPanel("input.toggle_outliers == true", numericInput(inputId = "threshold_toggle_outliers", "Threshold", value=20.67, min = 0, max = 100, step = 0.01)),
      checkboxInput(inputId = "toggle_lasso_outliers", "Select and remove outliers by box selection"), 
      conditionalPanel("input.toggle_lasso_outliers == true", actionButton("remove_lasso_points", "Remove lasso selection")),
      h4("Consistency checks"),
      checkboxInput(inputId = "negative_values", label = "Detect negative values", value = FALSE),
      checkboxInput(inputId = "detect_nonconstant_measurement_intervals", label = "Detect non-constant measurement intervals", value = FALSE),
      checkboxInput(inputId = "highly_varying_measurements", label = "Detect highly varying measurements", value = FALSE),
      conditionalPanel("input.highly_varying_measurements == true", sliderInput("threshold_for_highly_varying_measurements", "Threshold [%]", min = 0, max = 200, step = 10, value = 200)),
   ))


page_for_visualization <- fluidPage(
   useShinyjs(),
   fluidRow(
      column(8, style = "padding: 0px;",
      h4("Variable selection")),
   tabsetPanel(id = "tabsPC", type = "hidden",
      tabPanelBody("PC",
   selectInput(inputId = "ic_system", "Select indirect calorimetry platform", factor(c("General", "COSMED", "Sable"))),
   conditionalPanel(condition = "input.ic_system == 'General'", selectInput("plot_type", "Select quantity to plot", factor(c("Metadata", "RawMeasurement", "TotalHeatProduction", "RestingMetabolicRate", "HeatProduction", "FuelOxidation")))),
   conditionalPanel(condition = "input.ic_system == 'COSMED'", selectInput("plot_type", "Select quantity to plot", factor(c("Metadata", "RawMeasurement", "TotalHeatProduction", "RestingMetabolicRate", "HeatProduction", "FuelOxidation", "EstimateRMRforCOSMED", "CompareHeatProductionFormulas")))),
   conditionalPanel(condition = "input.ic_system == 'Sable'", selectInput("plot_type", "Select quantity to plot", factor(c("Metadata", "RawMeasurement", "TotalHeatProduction", "RestingMetabolicRate", "HeatProduction", "FuelOxidation", "Locomotion", "LocomotionBudget", "CompareHeatProductionFormulas")))),
   conditionalPanel(condition = "input.plot_type == 'RawMeasurement'", uiOutput("myr")),
   conditionalPanel(condition = "input.plot_type == 'FuelOxidation'", selectInput("goxlox", "FuelOxidation", choices = c("Glucose oxidation", "Lipid oxidation", "Fat oxidation", "Protein oxidation", "Nitrogen oxidation"))),
   conditionalPanel(condition = "input.plot_type == 'TotalHeatProduction' || input.plot_type == 'DayNightActivity'", selectInput("box_violin_or_other", "Type of visualization", c("Boxplot", "Violinplot", "Dotplot"), selected="Violinplot")),
   conditionalPanel(condition = "input.plot_type == 'DayNightActivity'", selectInput("box_violin_or_other", "Type of visualization", c("Boxplot", "Violinplot", "Dotplot"), selected="Boxplot")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("myp")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wmeans")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wmeans_choice")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wstats")),
   conditionalPanel(condition = "input.plot_type == 'HeatProduction'", uiOutput("wmethod")),
      ))))

page_for_visualization_grouping <- fluidPage(
   h4("Grouping and filtering"),
   checkboxInput(inputId = "with_grouping", label = "Select group and filter by condition"),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("condition_type")),
   conditionalPanel(condition = "input.with_grouping == true", uiOutput("select_data_by")),
   checkboxInput(inputId = "with_facets", label = "Select a group as facet"),
   conditionalPanel(condition = "input.with_facets == true", uiOutput("facets_by_data_one")),
   conditionalPanel(condition = "input.with_facets == true", selectInput("orientation", "Orientation", choices = c("Horizontal", "Vertical"))),
   checkboxInput(inputId = "select_temperature", label = "Select temperature"),
   conditionalPanel(condition = "input.select_temperature == true", sliderInput(inputId = "temperature_mean", label = "Temperature", min=0, max=30, value=4, step=1)),
   conditionalPanel(condition = "input.select_temperature == true", sliderInput(inputId = "temperature_deviation", label = "Deviation from temperature", min=0, max=1, value=0.05, step=0.05)),
   uiOutput("checkboxgroup_gender")
)

page_for_visualization_experimental_times <- fluidPage(
   h4("Experimental times"),
   checkboxInput(inputId = "timeline", label = "Annotate day/night light cycle"),
   conditionalPanel(condition = "input.use_zeitgeber_time != false",
   checkboxInput(inputId = "only_full_days_zeitgeber", label = "Select full days based on zeitgeber time", value = FALSE)),
   conditionalPanel(condition = "input.use_zeitgeber_time == false",
   checkboxInput(inputId = "only_full_days", label = "Select full consecutive calendrical days", value = FALSE)),
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
   selectInput("light_cycle", "Light phase selection", c("Day", "Night"), multiple = TRUE, selected = c("Day", "Night")),
)

page_for_visualization_advanced_options <- fluidPage(
   h4("Advanced options"),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", h3("Time Interval or Steady-State method to estimate RMR")),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", selectInput("rmr_method", "Method", choices = c("SS", "TI"))),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("rmr_method_frequency", "Frequency", min = 0, max = 30, value = 10)),
   conditionalPanel(condition = "input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("rmr_method_begin", "Duration", min = 3, max = 10, value = 3)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", h4("CVs")),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_VO2", "VO2", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_VCO2", "VCO2", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_RER", "RER", min = 0, max = 100, value = 5)),
   conditionalPanel(condition = "input.rmr_method == 'SS' && input.plot_type == 'EstimateRMRforCOSMED'", sliderInput("SS_method_VE", "VE*", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("window", "Window size", min = 1, max = 30, value = 5, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("rmr_averaging", "Averaging width", min = 1, max = 30, value = 1, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("percentage_best", "Fraction best", min = 1, max = 100, value = 1, step = 1)),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", selectInput("cvs", "Component:", choices = c("CO2", "O2"), multiple = FALSE, selected = "O2")),
   h5("Time averaging of measurements"),
   checkboxInput(inputId = "override_averaging", label = "Override averaging method (mean)"),
   conditionalPanel(condition = "input.override_averaging == true", selectInput("avg_method_for_statistics", "Method", choices = c("mean", "median"))),
   h5("Light cycle configuration"),
   checkboxInput(inputId = "override_metadata_light_cycle", label = "Override"),
   conditionalPanel(condition = "input.plot_type == 'RestingMetabolicRate'", sliderInput("threshold_light_day", "Light threshold (Day)", min = 0, max = 100, value = 10)),
   conditionalPanel(condition = "input.plot_type == 'TotalHeatProduction'", sliderInput("threshold_light_day", "Light threshold (Day)", min = 0, max = 100, value = 10)),
   sliderInput(inputId = "light_cycle_start", label = "Light cycle start", min = 0, max = 24, value = 6),
   sliderInput(inputId = "light_cycle_stop", label = "Light cycle stop", min = 0, max = 24, value = 18),
   colourInput(inputId = "light_cycle_day_color", label = "Color day", "#FFBF00"),
   colourInput(inputId = "light_cycle_night_color", label = "Color night", "#B2BEB5"),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", h5("Time averaging of raw data")),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("averaging", "Time averaging [min]", 0, 30, 10, step = 1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", sliderInput("running_average", "Moving average (k)", 0, 10, 1, step = 1)),
   conditionalPanel(condition = "input.plot_type != 'RestingMetabolicRate'", selectInput("running_average_method", "Method", choices = c("Mean", "Max", "Median", "Sum")))
)

page_for_visualization_control <- fluidPage(
   h4("Actions"),
   tags$style(HTML("
        #reset {
          background-color: #637DFF; /* Pastel Blue */
          color: white;
          //border-color: #637DFF;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #reset:hover {
          background-color: #A3B8FF;
          //border-color: #A3B8FF;
          border-color: white;
        }
        #plotting {
          background-color: #77DD77; /* Pastel Green */
          color: white;
          //border-color: #77DD77;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #plotting:hover {
          background-color: #5CB85C; 
          //border-color: #5CB85C;
          border-color: white;
        }
        #refresh {
         background-color: #8DBBD0; /* Pastel Light Blue */
         //border-color: #8DBBD0;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #refresh:hover {
         background-color: #6E98AA;
          border-color: white;
         //border-color: #6E98AA
        }
        #load_data { /* Pastel Red */
         background-color: #FF6961;
         //border-color: #FF6961;
          border-color: white;
          display: block;
          text-align: center;
          width: 150px;
        }
        #load_data:hover {
        background-color: #FFB3AB;
        //border-color: #FFB3AB;
          border-color: white;
        }
   ")),
   actionButton("load_data", "Reload data"), br(),
   actionButton("plotting", "Show plot",), br(),
   actionButton("refresh", "Refresh plot"), br(),
   actionButton("reset", "Reset session"), br(),
   h4("Defaults"),
   checkboxInput("use_default_plot_style", "Use default plot style", value=TRUE),
   checkboxInput("use_default_plot_style", "Use default color palette", value=TRUE),
)


sidebar_content2 <- fluidPage(
   useShinyjs(),
   tags$head(
    tags$style(HTML("
     .menu-button {
        display: block;
        width: 100%;
        //padding: 10px;
        //font-size: 16px;
        text-align: center;
        color: white;
        background-color: #283e4f;
        border-radius: 2px;
        border: none;
        cursor: pointer;
        transition: background 0.3s ease;
        text-decoration: none;
      }
      .menu-button:hover {
        background-color: #0056b3;
      }
      .active-button {
        background-color: #0056b3 !important;
        font-weight: bold;
      }
      /*#middle_panel {
        border: 1px solid #ddd;
        padding: 15px;
        border-radius: 5px;
        background-color: white;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
      }*/
      .section-content {
        display: none;  /* Hide all sections initially */
        //padding: 15px;
        //background-color: #f8f9fa;
        //border-radius: 5px;
        //border: 1px solid #ddd;
      }
    "))
  ),
   sidebarPanel(
      width=2,
      h3("Dataset import"),
      actionLink("toggleA_example", "Example data", class = "menu-button"),
      br(),
      actionLink("toggleA_custom", "Custom data", class = "menu-button"),
      br(),
      actionLink("toggleA_preprocessing", "Import options", class = "menu-button"),
      hr(),
      h3("Statistics and visualization"),
      actionLink("toggleB_variable_selection", "Variable selection", class = "menu-button"),
      br(),
      actionLink("toggleB_groups", "Grouping and filtering", class = "menu-button"),
      br(),
      actionLink("toggleB_experimental_times", "Experimental times", class = "menu-button"),
      br(),
      actionLink("toggleB_advanced_options", "Advanced options", class = "menu-button"),
      br(),
      actionLink("toggleB_control", "Plotting control", class = "menu-button"),
      hr(),
      h3("Data curation"),
      actionLink("toggleC_data_curation", "Trimming", class = "menu-button"),
      br(),
      actionLink("toggleC_data_curation_selection", "Select days and samples", class = "menu-button"),
      hr(),
      h3("Result summary"),
      actionLink("toggleD", "Download data", class = "menu-button"),
   ),
   column(2, id ="middle_panel", style="border: 1px solid #ddd;",
      div(id = "sectionA_example", class = "section-content",
         page_for_data_import_example_data,
      ),
      div(id = "sectionA_custom", class = "section-content",
         page_for_data_import
      ),
      div(id = "sectionA_preprocessing", class = "section-content",
         page_for_data_import_preprocessing,
         page_for_data_import_select_equation
      ),
      div(id = "sectionB_control", class = "section-content", 
         page_for_visualization_control
      ),
      div(id = "sectionB_variable_selection", class = "section-content",
         page_for_visualization
      ),
      div(id = "sectionB_groups", class = "section-content",
         page_for_visualization_grouping
      ),
      div(id = "sectionB_experimental_times", class = "section-content",
         page_for_visualization_experimental_times
      ),
      div(id = "sectionB_advanced_options", class = "section-content",
         page_for_visualization_advanced_options,
      ),
      div(id = "sectionC_data_curation", class = "section-content",
         page_for_data_curation
      ),
      div(id = "sectionC_data_curation_selection", class = "section-content",
         page_for_data_curation_selection
      ),
      div(id = "sectionD", class = "section-content",
         page_for_data_export
      )
   ),
      main_content
)


################################################################################
# Visualization panel
################################################################################
visualization <- tabPanel(
  "Analysis",
  #titlePanel("Energy expenditure of cohort studies using indirect calorimetry"),
  sidebar_content2,
  #sidebarLayout(
  #  sidebar_content2, main_content
  #),
# hidden session ID to manage global user data (or hidden; if required to hide)

   div(class="footer_version", 
    span(textOutput("session_id"), style = "font-size: 10px; visibility: visible;"),
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
      validation_panel, plotting_validation_panel
   )
)

################################################################################
# locomotion panel
################################################################################
locomotion_panel <- tabPanel(
   "Analyze locomotion",
   titlePanel("Analyze locomotion of Sable System experiments")
)

################################################################################
# documentation panel
################################################################################
documentation <- tabPanel(
      "Help",
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
      )),
  br(),
  br(),
  h3("Supported input file formats"),
  div(style="display: flex; justify-content: center",
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
   )),
   br(),
    span(textOutput("git_info"), style = "font-size: 10px; visibility: visible;")
   
 #  hr(style = "width:75%;"),
#   h1("List of selected app features"),
#   tags$table(class = "feature-table",
#      tags$tr(
#         tags$td(icon("timeline", "fa-3x")),
#         tags$td(h3("Long term observational studies")),
#         #tags$td(p("On a single data set. This description deals with an animal experiment investigating mice of two different genotypes (ko & wt) running in two cohorts (C1 & C2) and thus at two different timepoints. The provided commands are compatible with csv-files exported by the LabMaster software. Just export the respective csv-file and save it to a appropriate location.")) #nolint
#      ),
#      tags$tr(
#         tags$td(icon("plus-minus", "fa-3x")),
#         tags$td(h3("Time averaging for robust analysis")),
#        # tags$td(p("As calorimetry data is quite unstable, it is usually averaged over a certain time period (e.g. 1h as in CalR). However, one might to adjust this time period to personal preferences e.g. 30 minute intervals. This script offers this possibility. In order to do so, one must again extract some additional information form the data. In a first step, the cumulative running time of the experiment is calculated. The cumulative time (in s) is subsequently transformed in hours.")) #nolint
#
#      ),
#      tags$tr(
#         tags$td(icon("user-xmark", "fa-3x")),
#         tags$td(h3("Outlier detection and removal")),
#         #tags$td(p("Although not recommended you might want to exclude animals from the data. Maybe animals got sick, did not eat or technical issues with the cage occurred. One solution to remove whole animals is provided below.")) #nolint
#      ),
#      tags$tr(
#         tags$td(icon("folder-tree", "fa-3x")),
#         tags$td(h3("Combining several data sets")),
#        # tags$td(p("Limited capacity is often an issue for calorimetry, thus mice might be measured at different time points and need to be combined to a single data set. This is a issue since merging by time is not that easy due to slightly different starting time points etc. Be sure to always adjust your time period - the date can/should differ, however your starting period should always be in the same hour (e.g. 12:02:00 for the first data set and 12:00:00 for the second data set)")) #nolint
#      ),
#      tags$tr(
#         tags$td(icon("magnifying-glass-chart", "fa-3x")),
#         tags$td(h3("Visualization and analysis of data")),
#        # tags$td(p("Head over to the Visualization tab in the navigation bar at the top of this window - currently single data files are supported for analysis")) #nolint
#      ),
#      tags$tr(
#         tags$td(icon("file-export", "fa-3x")),
#         tags$td(h3("Data export")),
#        # tags$td(p("Various export data formats supported, for instance CalR compatible data export"))
#      )
#   ),
   #hr(style = "width:100%;")
)

################################################################################
# contact panel
################################################################################
contact <- tabPanel(
   "Contact",
   style = "text-align: center",
   titlePanel("Contact the developer of the application"),
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
   h2("Follow updates on our socials"),
   tags$table(class = "contact", style = "margin-left: auto; margin-right: auto",
      tags$tr(
         tags$td(tags$a(href = "http://github.com/stephanmg/CALOR", icon("fa-brands fa-square-github", "fa-3x"))),
         tags$td(tags$a(href = "http://twitter.com/smgrein", icon("fa-brands fa-square-twitter", "fa-3x"))),
      )
   ),
   h2("Documentation and tutorial videos"),
   tags$table(class = "contact", style = "margin-left: auto; margin-right: auto",
      tags$tr(
         tags$td(tags$a(href = "http://youtube.com/@CALOR-APP", icon("fa-brands fa-square-youtube", "fa-3x")))
      )
   )
)


################################################################################
# main navigation bar
################################################################################
ui <- tagList(
  tags$head(tags$title("CALOR - A web application for general indirect calorimetry analysis")),
  useShinyjs(),
  # Note that this is inline CSS and HTML code, can also be put in external files,
  # e.g. code.js for code and style.css for stylesheets
  #tags$head(
  # tags$style(HTML("
  # .logo-container {
  # position: absolute; top: 5px; left: 10px; z-index: 2147483647;
  # }
  # .navbar {
  #   padding-left: 0px;
  # }
  # "
  # ))
  #),
  #div(class="logo-container", img(src="shiny_logo.png", height="30px")),
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  #div(
  # align="right",
  # class = ".navbar .container-fluid",
  # tags$a(
  #    href="https://google.com",
  #    target="_blank",
  #    tags$i(class = "fab fa-github"),
  #    style="position: relative; top: 30px; left: 5px; z-index: 100000; font-size: 30px;"
  #)),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  navbarPage(
    id="navbar",
    header = list(use_cicerone()),
    theme = shinytheme("superhero"),
    title=NULL,
    #"CALOR - A reactive web-based application for the analysis of indirect calorimetry experiments",
    intro_panel,
    visualization,
    tabPanel("Metadata converter"),
    tabPanel("Documentation"),
    documentation,
    contact
  ),
  

)
