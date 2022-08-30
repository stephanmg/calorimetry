#!/bin/bash

BINARY=python3
SCRIPT=compare.py

for window in 5 10 25; do
   $BINARY $SCRIPT --file other_data/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref other_data/comp_table.tsv \
                   --output other_data \
                   --time 5 \
                   --name 'Bl6j_vs_Bl6n_TSE'

   $BINARY $SCRIPT --file old_data/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref old_data/comp_table.tsv \
                   --output old_data \
                   --time 10 \
                   --name 'Jnk1_Dusp8_KOonHFD_TSE_clean'
done
