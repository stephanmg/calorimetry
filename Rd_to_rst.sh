#!/bin/bash

# Set the directories
RD_DIR="man"             # Directory where .Rd files are located
MD_DIR="docs/md"         # Temporary directory for markdown files
RST_DIR="docs/rst"       # Directory where .rst files will be saved

# Create the markdown and rst directories if they do not exist
mkdir -p "$MD_DIR"
mkdir -p "$RST_DIR"

# Function to convert .Rd files to markdown using R
convert_rd_to_md() {
  echo "Converting .Rd files to markdown..."
  Rscript -e "
  library(Rd2md);
  rd_files <- list.files('$RD_DIR', pattern = '\\\\.Rd$', full.names = TRUE);
  for (rd_file in rd_files) {
    output_file <- file.path('$MD_DIR', sub('\\\\.Rd$', '.md', basename(rd_file)));
    Rd2md::Rd2markdown(rd_file, out = output_file);
  }
  "
}

# Function to convert markdown files to rst using pandoc
convert_md_to_rst() {
  echo "Converting markdown files to reStructuredText (.rst)..."
  for md_file in "$MD_DIR"/*.md; do
    # Get the base name of the markdown file
    base_name=$(basename "$md_file" .md)
    # Set the output rst file path
    rst_file="$RST_DIR/$base_name.rst"
    # Convert using pandoc
    pandoc "$md_file" -f markdown -t rst -o "$rst_file"
    echo "Converted $md_file to $rst_file"
  done
}

# Run the functions
convert_rd_to_md
convert_md_to_rst

echo "Conversion complete."
