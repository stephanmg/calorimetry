do_export_all_data
==================

Description
-----------

This function exports all data calculated during the app usage as an
archive

Usage
-----

.. code:: r

   do_export_all_data(
     input,
     output,
     session,
     file_output,
     do_plotting,
     global_data
   )

Arguments
---------

-  ``input``: shiny input
-  ``output``: shiny output
-  ``session``: shiny session
-  ``file_output``: output file path chosen by user
-  ``do_plotting``: plotting routines
-  ``global_data``: hash table containing variables during session
