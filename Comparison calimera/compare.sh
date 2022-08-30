#!/bin/bash

BINARY=python3.9
SCRIPT=compare.py

for window in 5 10 25; do
   $BINARY $SCRIPT --file other_data/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref other_data/comp_table.tsv \
                   --output other_data

   $BINARY $SCRIPT --file old_data/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref old_data/comp_table.tsv \
                   --output old_data
done
