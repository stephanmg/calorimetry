#!/bin/bash

for window in 5 10 25; do
   python3 compare.py --file df_for_comparison_with_calimera_$window.csv --window $window
done
