#!/bin/bash

# roxygenise only looks in R for files
if [ -d "R" ]; then
   # folder already exists
   echo
else
   mkdir R
fi

# copy all files to R folder
find inc/ -type f -name "*.R" -exec cp {} R \;
cp inc/util.R R

# generate Rd man files
Rscript generate_man.R

# convert to sphinx documentation rst files
./Rd_to_rst.sh

# populate api docs
./populate_api_docs.sh

# make html docs
cd docs
make html
