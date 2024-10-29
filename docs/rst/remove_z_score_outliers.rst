remove_z_score_outliers
=======================

Description
-----------

This function removes outliers based on multiples of the standard
deviation

Usage
-----

.. code:: r

   remove_z_score_outliers(df, sd = 1)

Arguments
---------

-  ``df``: data frame
-  ``sd``: multiple of the standard deviation (default=1)

Examples
--------

.. code:: r

   remove_z_score_outliers(values, sd=2)
