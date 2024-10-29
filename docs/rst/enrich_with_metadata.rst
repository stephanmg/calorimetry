enrich_with_metadata
====================

Description
-----------

Enrich data frame with metadata

Usage
-----

.. code:: r

   enrich_with_metadata(finalC1, C1meta, havemetadata, metadatafile)

Arguments
---------

-  ``finalC1``: data frame
-  ``C1meta``: metadata from data files in finalC1
-  ``metadatafile``: structured metadata sheet
-  ``havemetadatafile``: boolean to indicate if we have structured
   metadata sheet

Examples
--------

.. code:: r

   enrich_with_metadata(values, metadata_basic, TRUE, "metadata.xlsx")
