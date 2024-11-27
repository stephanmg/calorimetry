What is CALOR?
==============

CALOR is a web application based on Shiny R to allow streamlined visualization and statistical analysis of
indirect calorimetry data set making use of structured metadata to faciliate downstream analysis tasks.

For a very brief tour use the **User guide** button in the **Visualization and statistical analysis** tab
Short written examples are accessible through the **Getting help** button on the landing page.


Getting started
---------------

1. Loading and navigating the application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Navigate to `CALOR <https://shiny.iaas.uni-bonn.de/Calo>`_ access the Shiny web application.
The landing page displays some information about supported file formats and the app's main features, see
:ref:`calor_landing`. Click the **Visualization and statistical analysis** in the nagivation bar at the top
to get started. To get help or access a helper app use the **Getting help** or **Metadata converter** button.

.. _calor_landing:

.. figure:: img/calor_landing.png
   :align: center
   :alt: CALOR landing page
   :scale: 50%

   Figure 1: CALOR landing page


2. Loading indirect calorimetry data sets 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Jump ahead to the section **Conducting analyses with CALOR** to directly get started, or use the
**User guide** built into the application, see :ref:`load_example_data_set`.

2.1 Loading example data sets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Load example data sets and metadata, either **UCP1 KO** (4 cohort study) or **DAKO** (2 cohort study) are available. 
Metadata is automatically loaded and attached to the corresponding data set, see :ref:`load_example_data_set`.

.. _load_example_data_set:

.. figure:: img/01_a_intro_load_example_data_set.png
   :align: center
   :alt: Load example data set
   :scale: 50%

   Figure 1: Load example data set


2.2 Loading own data sets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that providing metadata is optional. Metadata can be added through the upload of a standardized and hierarchical
Excel Metadata Sheet (Seep et al., 2024, Scientific Data). Since the amount of metadata encoded in raw data file headers (short and non-standardized section
in the beginning of each data file exported from e.g. TSE Systems) is limited, some of the CALOR's functionality might not
be available for users providing only raw data sets. See :ref:`generate_metadata_sheet` below on how to generate 
standardized metadata for your data set(s).

To load your own data set, left-click on the large plus symbol.

.. _load_own_data_set:

.. figure:: img/01_b_intro_load_own_data_set.png
   :align: center
   :alt: Load own data set
   :scale: 50%
   
   Figure 2: Load own data set


If you already have the Metadata Sheet, simply tick **Have additional metadata?** in the top left section of the application,
and provide your individual cohorts (possible multiple) as file uploads by clicking on the **Browse...** button, see Figure :ref:`load_own_data_set_dialog`.
Adjust the **Number of data files** value according to your needs. Note that typically 2 or 4 cohorts are recorded per indirect calorimetry experiment.

.. _load_own_data_set_dialog:

.. figure:: img/01_b_intro_load_own_data_set_with_metadata.png
   :align: center
   :alt: Load own data set and metadata
   :scale: 50%

   Figure 3: File upload dialog for data and metadata


.. _generate_metadata_sheet:

3. Generate metadata sheet for indirect calorimetry data sets 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While this step is optional, we want to emphazise that generation of a Metadata Sheet for your cohort study has multiple 
benefits, i.e. statistical analysis of related metadata, comprehensive visualization of all collected metadata for the experiment,
streamlined statistical analysis and visualization of data sets supported by consistent metadata with corresponding units. 
(Reminder: For instance TSE Systems file headers provide limited and non-standardized metadata and is prone to unit and conversion
errors when combining cohort studies when not carefully exported from the PhenoMaster/LabMaster, also Metadata like Conditions
as cold exposure vs room temperate might be lacking).

If you already have filled out a Metadata Sheet (Seep et al., 2024, Scientific Data) for your indirect calorimetry experiment,
then you can skip this step, otherwise we encourage you to fill out either the full Metadata Sheet for your experiment (see 
the Excel Metadata Sheet template for indirect calorimetry data) or use the Metadata Sheet helper application to fill out 
the Metadata Sheet online if you have Excel not available. In the latter case navigate to `Metadata converter <https://shiny.iaas.uni-bonn.de/CaloHelper>`_. 
This application will allow you to fill out a Metadata Sheet and save it in Excel format for metadata input into CALOR.

First option: Provide an Excel (*.xlsx*) file with the following column structure to the Metadata converter:

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

You can display your input Excel sheet with the button **Display input Excel file** to confirm you adhere to the
required structure displayed in the example metadata table above, see :ref:`metadata_converter_landing`.

.. _metadata_converter_landing:

.. figure:: img/metadata_converter_landing.png
   :align: center
   :alt: Metadata converter landing page
   :scale: 50%

   Figure 4: Metadata converter


You can then download the metadata sheet by the download button **Download metadata sheet**.

Note that animal IDs (Animal #) need to be numeric, sex always specified as male or female, diet as a string, age at 
start must use the same unit, i.e. weeks or days, **bw** start, **bw** end, **fm** start, **fm** end, **lm** start and 
**lm** end correspond to the body weight, lean and fat mass at the start respectively end of the
experiment and to be reported in units of gram. All displayed columns are required. 

Second option: Manual fill-in of a Metadata Sheet by using the option **Specify metadata instead manually**. This will
guide you step by step through the input of your, e.g. 1, 2 or 4 cohorts study and collect the corresponding metadata
for each sample. Additional information, such as conditions (cold exposure vs room temperature) can be specified via the
*Condition* option which need to be enabled by a left-click on the **Enter study details** checkbox.

.. _metadata_converter_manual:

.. figure:: img/metadata_converter_manual.png
   :align: center
   :alt: Metadata converter landing page
   :scale: 50%

   Figure 4: Metadata converter manual input


You can then download the metadata sheet by the download button **Download metadata sheet**.

4. A Walk-through of CALOR's features
--------------------------------------------------------

Use the larger **+** (plus symbol) right to the **Plotting** section to expand options and see which quantities are available.

4.1 Inspect recorded metadata for your experiment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: First steps involve to display the metadata about the body composition and other metadata by chosing **Metadata**
from the **Select quantity to plot** selection field. 

4.2. Raw measurements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: Next steps are to inspect **Raw** quantities, **EnergyExpenditure**, **TotalEnergyExpenditure** and **RestingMetabolicRate**
through the same selection field **Select quantity to plot**.

4.3. Total heat production
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: Next steps are to inspect **Raw** quantities, **EnergyExpenditure**, **TotalEnergyExpenditure** and **RestingMetabolicRate**
through the same selection field **Select quantity to plot**.

4.4. Heat production
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: Next steps are to inspect **Raw** quantities, **EnergyExpenditure**, **TotalEnergyExpenditure** and **RestingMetabolicRate**
through the same selection field **Select quantity to plot**.

4.5. Resting Metabolic Rate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: Next steps are to inspect **Raw** quantities, **EnergyExpenditure**, **TotalEnergyExpenditure** and **RestingMetabolicRate**
through the same selection field **Select quantity to plot**.

4.6. Fuel oxidation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Next steps are to inspect **Raw** quantities, **EnergyExpenditure**, **TotalEnergyExpenditure** and **RestingMetabolicRate**
through the same selection field **Select quantity to plot**.


Note that on the right hand side a plot of the quantity of interest will be displayed, additional panels for **Statistical Analysis**,
**Modelling** and additional information about group comparisons are displayed.

During analysis, data might need further curation, e.g. exclusion of animals or recorded days. Use the **Data curation** 
panel analogue to the previous **Plotting** section to visualize and analyze results.

Note that all plots can be downloaded as high resolution vector or bitmap graphics by hovering over the plotting area,
a menu with options will appear at the top border of the plotting area then.


5. Conducting analyses with CALOR
---------------------------------

TODO: Add: Hypothesen aufstellen für die beiden Beispieldatensätze:
Gibt es Genotyp Effekte?
Dann TotalHeatProduction, HeatProduction, RestingMetabolicRate zeigen,
Dann ANOVAs
Dann ANCOVAs

5.1. Example data set I: UCP1 KO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: Add

5.2. Example data set II: DAKO KO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: Add

5.3. Own data set
~~~~~~~~~~~~~~~~~~~~~~

TODO: Add

6. Data export
--------------
To export combined data sets for all cohorts, data frames for plotting of results, and calculated quantities,
all data can be download through the **Data export** panel, choose the *.zip* download which downloads one
compressed file containing all data.


Tutorial videos
====================

Workflow and use-cases are documented in a series of short instructional YouTube videos demonstrating the features of the
web application: `@CALOR-APP <https://www.youtube.com/@CALOR-APP>`_.

TODO: Add short videos for all of the subsections above.


Flow charts
=============

Statistical analysis
--------------------

TODO: Add Entscheidungshilfe, welche Statistik muss berechnet oder genutzt werden. Eine Abbildung erzeugen hier.


Analysis of indirect calorimetry data
-------------------------------------

TODO: Add Entscheidungshilfe, was will ich analysieren, und muss dementsprechend in CALOR auswählen. Eine Abbildung erzeugen hier.
