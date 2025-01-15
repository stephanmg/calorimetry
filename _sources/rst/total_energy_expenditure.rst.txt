total_energy_expenditure
========================

Description
-----------

This function calculates the total energy expenditure

Usage
-----

.. code:: r

   total_energy_expenditure(
     finalC1,
     C1meta,
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
-  ``C1meta``: basic metadata
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

   total_energy_expenditure(values, basic_metadata, full_metadata, input, output, session, global_data, 1)
