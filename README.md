# Calorimetry

[![Shiny app deployment](https://github.com/stephanmg/calorimetry/actions/workflows/deploy-shiny.yml/badge.svg)](https://github.com/stephanmg/calorimetry/actions/workflows/deploy-shiny.yml)

## Problems:

### Data (.csv)
- File encoding wrong: Using UTF-8 symbols in ISO-8859 destroys data read in and print out 
- Cohort 2 file header wrongly assumed (Provided R script probably never worked for this file)
- Cohort 2 data in columns incompatible with RER averaging procedure...

### Code (.R)
- Did filtering of animals ever work? Comparison made by string but required scalar number
- Did merging of cohorts ever work? File headers not compatible in .csv files

## Demo
[TSE-CaloSys Data Analysis](https://calorimetry.shinyapps.io/calorimetry/)
