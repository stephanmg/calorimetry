#!/bin/bash

BINARY=python3.9
SCRIPT=compare.py

for window in 5 10 25; do
   $BINARY $SCRIPT --file df_for_comparison_with_calimera_$window.csv --window $window
done
