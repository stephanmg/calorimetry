Frequently Asked Questions
==========================

.. faq::

Q: I receive the error `Error: wrong sign in 'by' argument` when using my metadata sheet

A: Make sure to select all cohorts in the input file dialog, e.g. when two cohorts are recorded in your accompanying metadata sheet, you will need to provide all two cohorts in the app.

Q: I receive the error `Error: wrong sign in 'by' argument` or similar when using my metadata sheet

A: Make sure that there are no NaNs in your provided metadata sheet.

Q: I do not have a metadata sheet for my data sets. How can I create the metadata sheet?

A: Use the provided template for the metadata sheet in .xlsx format. Otherwise, navigate to the metadata-converter Shiny app and either enter data manually, or specify data by the lab Excel sheet.

Q: Why does my PhenoMaster column format throw an error?

A: Currently PhenoMaster is only supported in row format.

Q: Why do I receive an error `Error: [object Object]` in statistical testing panel without a metadata file?

A: Most likely the statistical method is not appropriate for the provided data sets. A metadata file will fix this error.
