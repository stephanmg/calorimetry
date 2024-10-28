#!/bin/bash

# roxygenise only looks in R for files
if [ -L "R" ] || [ -d "R" ]; then
   # link already exists
   echo
else
   ln -s inc R
fi

# generate Rd man files
Rscript generate_man.R

# convert to sphinx documentation rst files
./Rd_to_rst.sh

# make html docs
cd docs
make html
