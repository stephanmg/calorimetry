Frequently Asked Questions
==========================

Q1: I receive the error `Error: wrong sign in 'by' argument` or similiar when using my metadata sheet
-----------------------------------------------------------------------------------------------------

A1 a: Make sure to select all cohorts in the input file dialog, e.g. when two cohorts are recorded in your accompanying metadata sheet, you will need to provide all two cohorts in the app typically. Otherwise it may or may not lead to errors. The error then can occur sporadic, i.e. in the case of select full days, since after joining animal IDs are missing for counting of individual days.


A2 b: Make sure that there are no NaNs in your provided metadata sheet. That error will always occur if metadata sheet has any NaN.

A1 c: If you supplied multiple cohorts, make sure that the columns are identical across the cohorts. If not, you should place a tick mark in the checkbox `Common columns` in the Dataset Import section under Import Options.

Q2: I do not have a metadata sheet for my data sets. How can I create the metadata sheet?
-----------------------------------------------------------------------------------------

A2: Use the provided template for the metadata sheet in .xlsx format. Otherwise, navigate to the metadata-converter Shiny app and either enter data manually, or specify data by the lab Excel sheet.

Q3: Why does my PhenoMaster column format throw an error?
---------------------------------------------------------

A3: Currently PhenoMaster is only supported in row-orientated format.

Q4: Why do I receive an error `Error: [object Object]` in statistical testing panel without a metadata file?
------------------------------------------------------------------------------------------------------------

A4: Most likely the statistical method is not appropriate for the provided data sets. A metadata file will fix this error by providing groups for statistical comparison. Alternatively, prepare your TSE metadata header rows correctly, for e.g. Genotype, Diet, etc.

Q5: I receive an error `Error: [object Object]` in statistical testing panel for energy expenditure?
----------------------------------------------------------------------------------------------------

A5: Make sure, that you calculated first, TotalEnergyExpenditure and then RestingMetabolicRate. Omiting one of these calculations will lead to the received error.

Q6: Data file can't be read at all when metadata is provided with an error: `Error [object Object]` or `Argument is of length 0`?
---------------------------------------------------------------------------------------------------------------------------------

A6: The TSE header must be valid, e.g. Weight with `.` instead of `.`, and if metadata `Genotype` etc. is used in metadata sheet as well, data have to be match the TSE header (e.g. no other groups like mis-spelled `cntrl` in TSE header if in metadata sheet is cntl and wt for instance allowed). Either fix in app or otherwise do not export TSE header from phenomaster at all and only fill metadata sheet. Also, when providing TSE files, the header must be valid.

Q7: There are erratic RMR traces in my visualization, e.g. dense and highly varying
-----------------------------------------------------------------------------------

A7: This issue could stem from the fact that not all animal IDs are recorded in the metadata, i.e. present in one of the cohort files but not in the metadata sheet. This issue should only occur for RMR.


Q8: There are erratic RMR traces in my visualization, e.g. days are missing
---------------------------------------------------------------------------
A8: Make sure to not disable interpolate to regular grid. Interpolation should be enabled in multi-cohort studies with with varying measurement interval length, or for single-cohort studies where the measurement interval is not fixed because of long recording periods (e.g. >7 days).
