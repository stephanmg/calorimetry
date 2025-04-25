Key features
=====================================

On the left side the navigation panel indicates the workflow within Shiny-Calorie. The workflow is implictly assumed to follwo the order from top to bottom.

The individual steps for analysis within Shiny-Calorie are thus:

1. Dataset import
2. Statistics and visualization (Includes possibility to review metadata and detection of biases)
3. Data curation (Outlier correction, selection of subjects, days, trimming of experimental times) and
4. Result summary (For exporting of results in high quality graphics and data tables)

Note that on the right panel a plot of the quantity of interest will be displayed in the tab (Main plot). 
Additional panels for **Statistical testing**, **Details** of statistical testing and **Statistical model** are available.
The **Explanation** tab provides additional information and a description of which data is currently displayed.

During analysis, data might need further curation, e.g. exclusion of animals or recorded days. Use the **Data curation** panel.

Datasets can be filtered for temperature and photoperiod over time.

The section statistics and visualization provides the calculation of heat production (HP), total heat production (THP)
resting metabolic rate (RMR) and raw quantities as well as fuel oxidation (from here on ocassionally referred to as modalities).

All plots can be downloaded as high resolution vector or bitmap graphics by hovering over the plotting area on the right panel,
a menu with options will appear at the top border of the plotting area to select either PDF, PNG or SVG export.

The next paragraphs will provide an overview of the individual analysis panels.

1: Metadata analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The metadata panel allows users to get an overview of the metadata and summary statistics thereof which have been
recorded alongside the indirect Shiny-Calorieimetry experiment. Covariates like body weight, lean mass, fat mass can be statistically
(with significace tests) compared and visualized via appropriate plots (boxplots, regression plots).

Analysis of metadata should always be the starting point before conducting any analysis. Assessment of the quality and the
distributions of recorded quantities is vital to a consistent downstream analysis. For instance if there is a significant 
difference between two genotype groups', say KO and WT, body composition, i.e. fat mass, further analysis should take the 
information into account before drawing any conclusions.

2: Raw Measurements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Raw measurement panel visualize the raw measurements of the respiratory gases oxygen and carbon dioxide, in either saturation
in percentage or volume changes over time intervals (typically recording intervals in indirect Shiny-Calorieimetry experiments are
5 or 10 minutes, but users can interpolate to a finer or coarser time grid by using **Dataset import->Import options**).

Derived quantities, as for instance the RER (respiratory exchange ratio) can be (re-)calculated. If desired,
users can pre-smooth or coarsen the raw traces too (typically not required, but depends on your use case).

3: Total Heat Production 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The total heat production (THP) is the sum of :math:`THP = RMR+HP`, i.e. of resting metabolic rate and heat production (HP).
Alternatively one can interpret the THP as total energy expenditure (TEE) such that we have the sum :math:`TEE=RMR+EE`, which is 
defined by resting metabolic rate and heat production (including physical activity).

Time traces, facetted (grouped) plots, ANOVA and ANCOVA analysis, and modelling of the dependent variable via linear-mixed effect model (LME)
panel is available for all modalities recorded in the indirect Shiny-Calorieimetry experiment.

4: Heat Production
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Heat production (HP) is the non-RMR contribution to the THP. Same analysis methods as for THP apply for the HP panel.

5: Resting Metabolic Rate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Resting metabolic rate (RMR) is the non-activity contribution to the THP. Same analysis methods as for THP apply for the RMR panel as well.

6: Fuel Oxidation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Glucose and lipid oxidation are regarded under the umbrella term fuel oxidation. Fuel oxidation is an alternative way to 
visualize the utilization of glucose or lipid oxidation during heat production, similar as the RER can indicate which
component is mainly oxidized (Raw measurements). Same analysis methods as for THP are available for the fuel oxidation panel.


Visualization and statistical data analysis
===========================================

Example dataset I: UCP1 KO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. container:: highlight-box

   :math:`H_0`:
   There is no genotype-specific effect (WT vs UCP1 KO) on the total production or resting metabolic rate.

To either reject or accept the null hypothesis :math:`H_0` we can make use of Shiny-Calorie's features.

First, we do inspect the metadata recorded with this multi cohort study, to identify outliers or idiosyncrasies associated with the dataset.

.. _ucp1_ko_metadata:

.. figure:: img/metadata.png
   :align: center
   :alt: Metadata for multi cohort UCP1 KO data set
   :scale: 50%

   Figure 5: Metadata for multi cohort UCP1 KO data set

According to Fig. :ref:`ucp1_ko_metadata` distribution numbers (n) for Genotype and Diet are balanced, there is no 
irregularities for body composition (fat and lean mass) and also no problematic changes of body composition over the
time course of the experiment. The study entails only male samples (male) on a high fat diet (HFD). Since there are no
obvious issues with the metadata, we can proceed by inspecting the raw measurements as recorded from the metabolic phenotyping system.

Plotting the raw quantities can be a valid and important first diagnostic of consistency of the experiment. 
See Fig. :ref:`ucp1_ko_rer_with_outlier` in particular for the RER stratified by Genotype (WT vs KO).

.. _ucp1_ko_rer_with_outlier:

.. figure:: img/rer_with_outlier.png
   :align: center
   :alt: RER for UCP1 KO with outlier
   :scale: 50%

   Figure 6: RER for UCP1 KO with outlier

We identify an outlier which has an erratic RER time plot, and thus also an erratic oxygen and carbon dioxide curve,
since the dimensionless RER is defined as: :math:`RER=\frac{\dot{V}_{CO_{2}}}{\dot{V}_{O_{2}}} \in [0,1]`.

Therefore we proceed with the *Data Curation* panel in the bottom left of the application, we remove outlier **2547**.
Furthermore, to see genotype differences, we select grouping by facets for the genotype, resulting in the stratified plots
of RER as depicted in Fig. :ref:`ucp1_ko_rer_without_outlier`.

.. _ucp1_ko_rer_without_outlier:

.. figure:: img/rer_without_outlier.png
   :align: center
   :alt: RER for UCP1 KO without outlier 2547
   :scale: 50%

   Figure 7a: RER for UCP1 KO without outlier 2547

Instead of removing the outlier completely, we could trim the dataset (experimental times). From experience we know that 
at the beginning and end of an experiment we might have erratic gas exchange values recorded (as for instance samples
are handled at the beginning and end of an experiment, metabolic cage is opened and closed, sample acclimatisation to
the temperature-controlled environment), thus Shiny-Calorie provides the **Data curation** panel to trim these experimental times, 
select full days, or specific days in either zeitgeber time or calender time, see Fig. :ref:`trimming`.

.. _trimming:

.. figure:: img/trimming.png
   :align: center
   :alt: Trimming of experimental times
   :scale: 50%

   Figure 7b: Trimming of experimental times


Notice that there is no qualitative (significant) difference between the mean traces of RER for the two genotypes.
Displayed are mean and standard deviation ribbons as can be configure directly below the plot in the application
as a generalized additive model.

We can confirm the visual analysis by navigating to the *Statistical Testing* panel to conduct a 1-way ANOVA on day-averaged values of RER,
revealing no statistical significant difference for genotypes, see Fig. :ref:`rer_anova`. We employ for this the *Wilcoxon-test*
as a non-parametric test since we are operating in the low *n* regime, otherwise also a *t-test* could be selected by the user. 

Other tests for post-hoc analysis can be selected in the application directly above the plot panel. Multiple-testing corrections (Bonferonni, etc.) 
can be selected when conducting higher-order ANOVAs, which we do not require here since our only factor is the genotype with two levels for one factor.

.. _rer_anova:

.. figure:: img/1_way_anova_rer.png
   :align: center
   :alt: 1-way ANOVA on genotype stratification for RER
   :scale: 50%

   Figure 8: 1-way ANOVA on genotype stratifiction for RER


Since outliers are now removed, we can revisit the total heat production in order to answer our null hypothesis.

.. container:: highlight-box

   :math:`Conclusion`:
   There is no genotype-specific effect (WT vs UCP1 KO) on neither THP or RMR.

Since there are no changes in THP or RMR when considering only the genotype, we
want now to consider also the case that during the experiment recorded covariates, i.e.
lean mass and fat mass (or changes therefore, in the following we use the terms
delta lm and delta fm) or the whole body weight (or delta bw) are available. Their influence
on the THP and RMR in the KO and WT genotype should be analyzed. To factor this into our
statistical model, we will make use of 1-way ANCOVAs during our next analyses.

In layman's terms the 1-way ANCOVA model is formulated as follows, where 
the dependent variable :math:`\text{DependentVar}`, the covariate :math:`\text{Covariate}`, 
and the grouping variable :math:`\text{Group}` appear in the model as:

.. math::

   \text{DependentVariable}_{ij} = \mu + \tau_i + \beta (\text{Covariate}_{ij} - \overline{\text{Covariate}}) + \epsilon_{ij}

The variables in the euqation are defined by:

- :math:`\mu` is the overall mean.
- :math:`\tau_i` is the effect of the :math:`i`-th group (:math:`\text{Group}`).
- :math:`\beta` is the regression coefficient for the covariate :math:`\text{Covariate}`.
- :math:`\text{Covariate}_{ij}` is the value of the covariate :math:`\text{Covariate}` for observation :math:`j` in group :math:`i`.
- :math:`\overline{\text{Covariate}}` is the mean of the covariate :math:`\text{Covariate}` across all observations.
- :math:`\epsilon_{ij}` is the random error term.

In particular for this model:

- The covariate :math:`\text{Covariate}` is adjusted by subtracting its mean (:math:`\overline{\text{Covariate}}`), centering it to reduce multicollinearity.
- The :math:`\beta` term measures the relationship between the covariate :math:`\text{Covariate}` and the dependent variable :math:`\text{DependentVariable}`.

The ANCOVA tests whether the group effects :math:`\tau_i` are significant while controlling for the covariate :math:`\text{Covariate}`.
Whether test assumptions are met (for ANCOVA we also need homogeneity of regression slopes in addition to the usual assumptions of homoscedasticity and normality of ANOVA),
is reported in the **Details** panel. Test statistics, and in particular p-values, are reported also in the **Details** panel. Statistical testing is configured in the homonymous panel.


Thus our first null hypothesis for the RMR can be stated as:

.. container:: highlight-box

   :math:`H_0`:
   There is a genotype-specific effect (WT vs UCP1 KO) on RMR corrected for one 
   of the covariates (lm, fm or bw)

Likewise an analogue null hypothesis for the THP can be stated as:

.. container:: highlight-box

   :math:`H_0`:
   There is a genotype-specific effect (WT vs UCP1 KO) on THP corrected for one 
   of the covariates (lm, fm or bw)


Now, we will proceed as before, but make use of 1-way ANCOVAs for THP 
(inluding either lm, fm or bw) and RMR (including either lm, fm or bw)
grouped by genotype KO and WT to investigate if body weight composition
might be a confounding factor in observing the true genotype effect.

For RMR see Figs. :ref:`rmr_ancova`, :ref:`rmr_ancova_details` and THP see Figs. :ref:`thp_ancova`, :ref:`thp_ancova_details`.

.. _thp_ancova:

.. figure:: img/ancova_ucp1ko_thp_details.png
   :align: center
   :alt: 1-way ANOVA on genotype stratification for THP with lean mass as covariate
   :scale: 50%

   Figure 9: 1-way ANCOVA on genotype stratifiction for THP lean mass as covariate


.. _thp_ancova_details:

.. figure:: img/ancova_ucp1ko_thp.png
   :align: center
   :alt: 1-way ANOVA on genotype stratification for THP with lean mass as covariate with statistical details
   :scale: 50%

   Figure 10: 1-way ANCOVA on genotype stratifiction for THP lean mass as covariate with statistical details


.. _rmr_ancova:

.. figure:: img/ancova_ucp1ko_rmr.png
   :align: center
   :alt: 1-way ANOVA on genotype stratification for RMR with lean mass as covariate
   :scale: 50%

   Figure 9: 1-way ANCOVA on genotype stratifiction for RMR lean mass as covariate


.. _rmr_ancova_details:

.. figure:: img/ancova_ucp1ko_rmr_details.png
   :align: center
   :alt: 1-way ANOVA on genotype stratification for RMR with lean mass as covariate with statistical details
   :scale: 50%

   Figure 10: 1-way ANCOVA on genotype stratification for RMR lean mass as covariate with statistical details


.. container:: highlight-box

   **Final conclusion**:
   Based on our analysis we can reject all of our posed null hypotheses for the UCP1 KO cohort study.


Example data set II: DAKO KO
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. container:: highlight-box

   :math:`H_0`:
   There is a genotype-specific effect on the  production or resting metabolic rate.

We will proceed as before with the analysis of this dataset. The DAKO KO data set is a two cohort study of
a double knockout experiment.

As before we get an overview of the metadata recorded with the experiment, see Fig. :ref:`dako_metadata` and
enspect the raw measurement, e.g. the RER, see Fig. :ref:`dako_rer` and detect not obvious outlier, so we
can use all samples from this dataset.

.. _dako_metadata:

.. figure:: img/dako_metadata.png
   :align: center
   :alt: Metadata for 2 cohort DAKO data set
   :scale: 50%

   Figure 11: Metadata for 2 cohort DAKO data set


.. _dako_rer:

.. figure:: img/dako_rer.png
   :align: center
   :alt: RER for DAKO data set
   :scale: 50%

   Figure 12: RER for DAKO data set


We proceed by calculating the total heat production, see Fig. :ref:`dako_thp` stratified by genotype.
After inspecting of the data, no outlier seems to be present. Next, we use in the *Statistical testing* 
panel a 1-way ANOVA which detects significant changes as indicated by the Wilcoxon test p-value smaller than 0.05
in the plot, see Fig. :ref:`dako_anova`.

.. _dako_thp:

.. figure:: img/dako_thp.png
   :align: center
   :scale: 50%
   :alt:  production for DAKO data set

   Figure 12:  production for DAKO data set


.. _dako_anova:
 
.. figure:: img/dako_anova.png
   :align: center
   :scale: 50%
   :alt: 1-way ANOVA for DAKO and  production

   Figure 13: 1-way ANOVA for DAKO and  production


Next we ask, where these effects could possibly be stemming from, and conduct thus multiple 1-way ANCOVA where we
adjust for the body weight, lean mass and fat mass respectively, see Fig. :ref:`dako_ancova_lean`, :ref:`dako_ancova_fat`, :ref:`dako_ancova_bw`.


.. _dako_ancova_bw:

.. figure:: img/dako_ancova_bw.png
   :align: center
   :scale: 50%

   Figure 14: 1-way ANCOVA for DAKO and  production adjusted for body weight


.. _dako_ancova_lean:

.. figure:: img/dako_ancova_lean.png
   :align: center
   :scale: 50%

   Figure 15: 1-way ANCOVA for DAKO and  production adjusted for lean mass


.. _dako_ancova_fat:

.. figure:: img/dako_ancova_fat.png
   :align: center
   :scale: 50%

   Figure 16: 1-way ANCOVA for DAKO and  production adjusted for fat mass


We do confirm with the *Statistical testing* and *Details* panel, that in fact the
effect seems to stem from the body composition in fat mass, see. Fig. :ref:`dako_thp_details`,
and not from the lean mass or total body weight.

.. _dako_thp_details:

.. figure:: img/ancova_thp_details.png
   :align: center
   :scale: 50%
   :alt: Statistics for 1-way ANCOVA of  production adjusted for fat mass

   Figure 17: Statistics for 1-way ANCOVA of  production adjusted for fat mass


Thus we are able to conclude the following:

.. container:: highlight-box

   :math:`Conclusion`:
   There is a genotype-specific effect (WT vs DAKO) on the  production
   mediated by the body composition's covariate fat mass.


Next, we will investigate the same hypothesis but now for the resting metabolic rate.

We start by a 1-way ANOVA for the two genotypes, which gives us a non-significant
difference between RMR, see Fig. :ref:`dako_rmr_anova`.

.. _dako_rmr_anova:

.. figure:: img/anova_rmr_dako.png
   :align: center
   :scale: 50%
   :alt: Statistics for 1-way ANOVA of resting metabolic rate

   Figure 18: Statistics for 1-way ANOVA of resting metabolic rate

Also we do not see significant differences, we are prompted to investigate
if lean, fat or body weight are confounding the analysis on RMR. As before
we perform multiple 1-way ANCOVAs with lean, fat and body weight each as
a covariate. 

We find that, genotype can be separated when chosing fat mass as a covariate,
see Fig. :ref:`dako_rmr_ancova`.

.. _dako_rmr_ancova:

.. figure:: img/ancova_rmr_dako.png
   :align: center
   :scale: 50%
   :alt: Statistics for 1-way ANCOVA of resting metabolic rate

   Figure 19: Statistics for 1-way ANCOVA of resting metabolic rate

Despite the visual separability, statistical testing reveals no significant
difference in mean resting metabolic rates, see Fig. :ref:`dako_rmr_ancova_details`.

.. _dako_rmr_ancova_details:

.. figure:: img/ancova_rmr_dako_details.png
   :align: center
   :scale: 50%
   :alt: Statistics for 1-way ANCOVA of resting metabolic rate

   Figure 20: Statistics for 1-way ANCOVA of resting metabolic rate

Thus we are able to conclude the following:

.. container:: highlight-box

   :math:`Conclusion`:
   There is no genotype-specific effect (WT vs DAKO) on the resting metabolic rate.


Custom data sets
~~~~~~~~~~~~~~~~~~~~~~

For own dataset or studies, users can make of the workflows as detailed in the
two use cases for the example datasets from above. 

In general, at the very beginning hypotheses should be posed for the experiment,
then consistency checks (metadata and raw data should be inspected,
and outliers be removed) should be applied. Afterwards quantities should be visualized
and analyzed, in the suggested order: TotalHeatProduction, RestingMetabolicRate,
HeatProduction and FuelOxidation.

All data frames calculated in Shiny-Calorie can be downloaded as a single compressed **.zip** file
in the *Export Data* section. Indidivual plots can be downloaded directly from the top
right corner in the corresponding plot, and saved as **.svg**, **.pdf** or **.png**.


Data export
~~~~~~~~~~~
To export combined datasets for all cohorts, data frames for plotting of results, and calculated quantities,
all data can be download through the **Data export** panel, choose the *.zip* download which downloads one
compressed file containing all data frames and plots.


Advanced use-cases
~~~~~~~~~~~~~~~~~~

For advanced use case, e.g. locomotional analysis and budgeting as well as wavelet analysis to study ultradian rhythms,
we refer to the supplementary material provided in the preprint [TODO: insert DOI here].



