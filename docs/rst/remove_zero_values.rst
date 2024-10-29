remove_zero_values
==================

Description
-----------

This function removes zero measurement values, based on epsilon
threshold

Usage
-----

.. code:: r

   remove_zero_values(df, eps)

Arguments
---------

-  ``df``: data frame
-  ``eps``: epsilon threshold

Examples
--------

.. code:: r

   remove_zero_values(values, 0.0001)
