# Calorimetry

[![Shiny app deployment](https://github.com/stephanmg/calorimetry/actions/workflows/deploy-shiny.yml/badge.svg)](https://github.com/stephanmg/calorimetry/actions/workflows/deploy-shiny.yml)

## Problems:

### Data (.csv)
- File encoding wrong: Using UTF-8 symbols in ISO-8859 destroys data read in
- Cohort 2 file header wrongly assumed (Provided R script probably never worked for this file)
