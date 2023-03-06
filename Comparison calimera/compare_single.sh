#!/bin/bash


BINARY=python3.9
case "$(uname -s)" in
   Linux*) BINARY=python3;;
esac

SCRIPT=compare_between_our_method.py

$BINARY $SCRIPT --file df_for_comparison_with_calimera_day.csv \
                   --window 5 \
                   --ref df_for_comparison_with_calimera_night.csv 
