energy_expenditure
==================

Description
-----------

This function calculates the energy expenditure

Usage
-----

.. code:: r

   energy_expenditure(
     finalC1,
     finalC1meta,
     input,
     output,
     session,
     global_data,
     scaleFactor
   )

Arguments
---------

-  ``finalC1``: input data
-  ``finalC1meta``: combined metadata
-  ``input``: shiny input
-  ``output``: shiny output
-  ``session``: shiny session
-  ``global_data``: dictionary to store variables session-based for
   users
-  ``scaleFactor``: used to scale energy expenditure units correctly

Examples
--------

.. code:: r

   energy_expenditure(values, full_metadata, input, output, session, global_data, 1)
