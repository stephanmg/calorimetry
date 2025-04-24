# General help for Shiny-Calorie

`Welcome to Shiny-Calorie, an application for the visualization and analysis of indirect Shiny-Calorieimetry data`.
<img src="../www/logo_shiny.svg" align="left" width="50" height="70" style="padding-right: 10px">
Please have a look at the overview of functions currently implemented in Shiny-Calorie on the main page,
i.e. navigate to the landing page `Introduction and Features` in the top navigation panel.
<br clear="left"/>
<hr style="width: 75%;"/>

Below we detail in a nutshell how to use the Shiny-Calorie application. If you need technical
assistance, found bugs or have feature requests, feel free to reach out by email,
see the `Contact` panel.

## Input data

Shiny-Calorie supports a variety of input data from different metabolic phenotyping platforms, 
e.g. Promethion (Live) by Sable Systems,
TSE PhenoMaster/LabMaster from TSE Systems and COSMED data. Supported file types:

| Name | Platform | File type | Version |
| ----------------------------- | ------------- | ----------- | ------- |
| PhenoMaster                   | TSE           | .csv / .tsv | v7, v8  |
| LabMaster                     | TSE           | .csv / .tsv | v5, v6  |
| -                             | COSMED        | .xlsx       | N/A     |
| Promethion (Live)             | Sable Systems | .xlsx       | N/A     |


## Analysis workflow
To conduct an analysis there are two cases, single cohort or multiple cohorts are available.

### Single Cohort Analysis
Steps:
1. Navigate to the `Visualization and statistical analysis`
2. Take the guided tour (Click button `Guide`)

Note that the individual panels (Energy expenditure, Plot configuration, Data curation and data export) 
can be collapsed or expanded using the 
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/regular/square-minus.svg" width="25" height="25">
and 
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/regular/square-plus.svg" width="25" height="25">
symbols.

3. Input cohort data through file chooser `Cohort 1`.
4. Adjust analysis settings as demonstrated in the guided tour from step number two
5. Click `Show` or `Reset` button in plotting control for visualization or to reset changes made
6. (Optional) repeat with another data set

### Multiple Cohort Analysis

In contrast to the Single Cohort Analysis, adjust the number of files through the
input field `Number of data files` in the `Energy expenditure` panel. 

Additionally in the panel `Plot configuration` you can stratify cohorts by available
metadata, e.g. genotypes, conditions, treatments, etc. if you wish. 

## Output data

Two available output options are available for data export.

1. Data output is supported into CalR compatible <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/file-csv.svg" width="25" height="25"> and  <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/regular/file-excel.svg" width="25" height="25"> files consolidating multiple cohorts when available which can be used in downstream analysis.

2. Figures can be exported in high quality publication-ready vector graphics  <img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/vector-square.svg" width="25" height="25"> (.svg) or raster graphics.
