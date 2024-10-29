Introduction
============

For a very brief tour use the **User guide** button in the **Visualization and statistical analysis** tab
Short written examples are accessible through the **Getting help** button on the landing page.


Getting started
---------------


1. Loading and navigating the application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Navigate to `CALOR <https://shiny.iaas.uni-bonn.de/Calo>`_ access the Shiny web application.
The landing page displays some information about supported file formats and main features.

Jump ahead to the section **Visualization and statistical analysis** to get started.

2. Loading your indirect calorimetry data sets and metadata
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Load example data sets and metadata, either **UCP1 KO** or **DAKO** are available. Alternatively, provide your own data set and metadata. 

   First note, metadata is optional, but some analysis functions are not available, since indirect calorimetry
   raw data sets allow only for a limited number of metadata. 

   If you then have the Metadata Sheet, simply tick **Have additional metadata?** in the top left section of the application.
   Provide simply the file through the upload dialog by clicking the **Browse...** button.

   Next provide *all* of your indirect calorimetry data sets associated with the metadata respectively your experiment.
   Use the **Number of data files** slider, to adjust to the number of cohorts you have in your experiment, e.g. typically
   we have 2 or 4 cohorts recorded.

3. Generate metadata sheet for indirect calorimetry data sets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   If you already have the filled out Metadata Sheet (Seep et al., 2024), then you
   can skip the this step, otherwise fill out either the full Metadata Sheet for your experiment or
   use the Metadata Sheet helper application to fill out the Metadata Sheet. In the latter case navigate
   to `Metadata converter <https://shiny.iaas.uni-bonn.de/CaloHelper>`_.

   You have two options: Automatic Metadata Sheet generation or a manual fill in.

   First option: Provide an Excel (*.xlsx*) file with the following column structure:

   Example metadata table for metadata converter
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   +-----------+--------+------+----------+-------------+----------+---------+----------+--------+----------+--------+
   | Animal #  | sex    | diet | genotype | age at start| bw start | bw end  | fm start | fm_end | lm start | lm end |
   +===========+========+======+==========+=============+==========+=========+==========+========+==========+========+
   | 1         | male   | HFD  | KO       | 11          | 20       | 23      | 5        | 6      | 15       | 17     |
   +-----------+--------+------+----------+-------------+----------+---------+----------+--------+----------+--------+
   | 2         | female | CD   | UCP1     | 11          | 22       | 25      | 7        | 8      | 16       | 18     |
   +-----------+--------+------+----------+-------------+----------+---------+----------+--------+----------+--------+
   | ...       | ...    | ...  | ...      | ...         | ...      | ...     | ...      | ...    | ...      | ...    |
   +-----------+--------+------+----------+-------------+----------+---------+----------+--------+----------+--------+

   You can display your input excel sheet with the button **Display input Excel file** to confirm you adhere to the
   required structure displayed in the example metadata table above.

   You can then download the metadata sheet by the download button **Download metadata sheet**.

   Note that animal IDs (Animal #) need to be numeric, sex always specified as male or female, diet as a string, age at 
   start must use the same unit, i.e. weeks or days, **bw** start, **bw** end, **fm** start, **fm** end, **lm** start and 
   **lm** end correspond to the body weight, lean and fat mass at the start respectively end of the
   experiment and to be reported in units of gram. All displayed columns are required. 

   Second option: Manual fill in of a Metadata Sheet by using the option **Specify metadata instead manually**. This wiill
   guide you step by step through the input of your, e.g. 4 cohorts and the corresponding metadata. Genotype can be added
   via the *Condition* option. 

   You can then download the metadata sheet by the download button **Download metadata sheet**.

4. Conduct analysis: Visualization and statistical analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Use the larger **+** (plus symbol) right to the **Plotting** section to expand options.

   First steps involve to display the metadata about the body composition and other metadata by chosing **BodyComposition**
   from the **Select quantity to plot** selection field. 

   Next steps are to inspect **Raw** quantities, **EnergyExpenditure**, **TotalEnergyExpenditure** and **RestingMetabolicRate**
   through the same selection field **Select quantity to plot**.

   Note that on the right hand side a plot of the quantity of interest will be displayed, additional panels for **Statistical Analysis**,
   **Modelling** and additional information about group comparisons are displayed.

   During analysis, data might need further curation, e.g. exclusion of animals or recorded days. Use the **Data curation** 
   panel analogue to the previous **Plotting** section to visualize and analyze results.


5. Data export
~~~~~~~~~~~~~~
   To export combined data sets for all cohorts, data frames for plotting of results, and calculated quantities,
   all data can be download through the **Data export** panel, choose the *.zip* download which downloads one
   compressed file containing all data.
 

Additional resources
--------------------

Workflow and use-cases are documented in a series of short instructional YouTube videos demonstrating the features of the
web application: `@CALOR-APP <https://www.youtube.com/@CALOR-APP>`_.






