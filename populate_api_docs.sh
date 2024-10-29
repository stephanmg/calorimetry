#!/bin/bash

cat > docs/api.rst << EOL
API Documentation
=================

.. toctree::
   :maxdepth: 2

EOL

find docs/rst/ -type f -name "*.rst" | while read -r file; do
   filename=$(basename $file .rst)
   echo "   rst/$filename" >> docs/api.rst
done
