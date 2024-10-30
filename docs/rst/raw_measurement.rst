raw_measurement
===============

Description
-----------

This function displays raw measurements

Usage
-----

.. code:: r

   raw_measurement(
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

   raw_measurement(values, full_metadata, input, output, session, global_data, 1)
