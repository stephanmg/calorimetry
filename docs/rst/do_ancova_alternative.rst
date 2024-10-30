do_ancova_alternative
=====================

Description
-----------

This function performs multi-way ANCOVA or ANOVA analysis

Usage
-----

.. code:: r

   do_ancova_alternative(
     df_data,
     df_metadata,
     indep_var,
     indep_var2,
     group,
     group2,
     dep_var,
     test_type,
     adjust_method = "bonferroni",
     connected_or_independent_ancova = FALSE,
     num_covariates = 1
   )
