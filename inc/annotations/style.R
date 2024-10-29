################################################################################
#' add_stylize_plot_plotting_control
#' 
#' This function adds UI elements to allow to customize the plot appearance
################################################################################
add_stylize_plot_plotting_control <- function() {
	return(
		tagList(
			h3("Plotting control"),
			checkboxInput("stylize_plot_general", "General"),
			checkboxInput("stylize_plot_axes", "Axes"),
			checkboxInput("stylize_plot_theme_and_title", "Theme, title and legend"),
			conditionalPanel("input.stylize_plot_general == true",
				selectInput("stylize_plot_general_font_family", "Font family", choices=c("Arial", "Times New Roman", "Courier New", "Verdana", "Georgia"), selected="Arial"),
				numericInput("stylize_plot_general_dpi", "DPI", min=100, max=300, value=300, step=10),
				numericInput("stylize_plot_general_width", "Plot width", min=1239, max=2400, value=1239, step=1),
				numericInput("stylize_plot_general_height", "Plot height", min=400, max=1200, value=400, step=1),
				numericInput("stylize_plot_general_dotsize", "Point size", min=1, max=32, value=6, step=1),
				numericInput("stylize_plot_general_linewidth", "Line width", min=1, max=32, value=6, step=1),
				selectInput("stylize_plot_general_palette", "Color palette", selected="default", choices=c("default", "rainbow", "bw"))
			),
			conditionalPanel("input.stylize_plot_axes == true", 
				selectInput("stylize_plot_axes_components", "Components to configure", multiple=TRUE, selected=c("Limits", "Labels", "Ticks"), choices=c("Limits", "Labels", "Ticks")),
			),
			conditionalPanel("input.stylize_plot_axes == true && (input.stylize_plot_axes_components.indexOf('Limits') > -1)",
				h4("Limits"),
				fluidRow(
					column(6,
					h5("x-axis limits"),
					checkboxInput("auto_scale_rmr_plot_limits_x", "Auto-scale", value = TRUE),
					numericInput("x_min_rmr_plot", "min", value = 0, min = 0),
					numericInput("x_max_rmr_plot", "max", value = 100, max = 100)
					),
					column(6,
					h5("y-axis limits"),
					checkboxInput("auto_scale_rmr_plot_limits_y", "Auto-scale", value = TRUE),
					numericInput("y_min_rmr_plot", "min", value = 0, min = 0),
					numericInput("y_max_rmr_plot", "max", value = 100, max = 100)
					)
				),
				hr(style="width: 75%")
			),
			conditionalPanel("input.stylize_plot_axes == true && (input.stylize_plot_axes_components.indexOf('Labels') > -1)",
				h4("Labels"),
				fluidRow(
					column(6,
					h5("x-axis label"),
					numericInput("stylize_plot_axes_x_axis_font_size", "Font size (in)", min=1, max=32, step=1, value=12),
					colourInput("stylize_plot_axes_x_axis_color", "Color", value="black"),
					textInput("stylize_plot_axes_x_axis_label", "Label text for x-axis")
					),
					column(6,
					h5("y-axis label"),
					numericInput("stylize_plot_axes_y_axis_font_size", "Font size (in)", min=1, max=32, step=1, value=12),
					colourInput("stylize_plot_axes_y_axis_color", "Color", value="black"),
					textInput("stylize_plot_axes_y_axis_label", "Label text for y-axis")
					)
				),
				hr(style="width: 75%")
			),
			conditionalPanel("input.stylize_plot_axes == true && (input.stylize_plot_axes_components.indexOf('Ticks') > -1)",
				h4("Ticks"),
				selectInput("stylize_plot_axes_ticks_major_minor", "Major and minor grid lines", multiple=TRUE, selected=c("Minor", "Major"), choices=c("Minor", "Major")),
				checkboxInput("stylize_plot_axes_ticks_disable_ticks", "Disable ticks"),
				fluidRow(
					column(6,
						h5("x-axis ticks"),
						numericInput("stylize_plot_axes_x_axis_tickfont_size", "Font size for tick labels (in)", min=1, max=32, step=1, value=12),
						numericInput("stylize_plot_axes_ticks_x_ticks", "Increase or decrease number of ticks", min=0, value = 1, max=10, step=0.01),
						numericInput("stylize_plot_axes_ticks_x_ticks_length", "Length of ticks", min=1, value = 1, max=10, step=0.5),
						colourInput("stylize_plot_axes_x_axis_tick_color", "Color", value="black"),
					),
					column(6,
						h5("y-axis ticks"),
						numericInput("stylize_plot_axes_y_axis_tickfont_size", "Font size for tick labels (in)", min=1, max=32, step=1, value=12),
						numericInput("stylize_plot_axes_ticks_y_ticks", "Increase or decrease number of ticks", min=0, value = 1, max=10, step=0.01),
						numericInput("stylize_plot_axes_ticks_x_ticks_length", "Length of ticks", min=1, value = 1, max=10, step=0.5),
						colourInput("stylize_plot_axes_y_axis_tick_color", "Color", value="black"),
					)
				),
				hr(style="width: 75%")
			),
			conditionalPanel("input.stylize_plot_theme_and_title", 
				h4("Title"),
				numericInput("stylize_plot_theme_and_title_title_font_size", "Font size (in)", min=1, max=32, step=1, value=12),
				colourInput("stylize_plot_theme_and_title_title_color", "Color", value="black"),
				textInput("stylize_plot_theme_and_title_title_label", "Title label"),
				hr(style="width: 75%"),
				h4("Theme"),
				selectInput("stylize_plot_theme_and_title_theme", "Theme", selected="minimal", choices=c("minimal", "bw", "dark", "white")),
				numericInput("stylize_plot_theme_and_title_scale_factor", "Overall scale factor for fonts", value=1.0, min=0, max=10.0, step=0.1),
				hr(style="width: 75%"),
				h4("Legend"),
				numericInput("stylize_plot_theme_and_title_legend_size", "Font size (in)", min=1, max=32, step=1, value=0),
				colourInput("stylize_plot_theme_and_title_legend_color", "Color", value="black"),
				textInput("stylize_plot_theme_and_title_legend_label", "Legend title")
			)
		)
	)
}