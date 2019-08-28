#!/usr/bin/env Rscript
options(stringAsfactors = FALSE, useFancyQuotes = FALSE)

# Taking the command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Load required libraries
library(stringr)
library(stringi)

# Set files and folders
parentFolder <- "/vol/metfamily/classyfire"
annoFileAll        <- ""
annoFileAllPresent <- "/vol/metfamily/classyfire/181019_MSMS_merge_HR_scaffolds.tsv"

setOfInChIs <- readLines(con = "/vol/metfamily/classyfire/190522_AllInchis_HR.txt")

setOfInChIsNotClassyFireable <- classyFireQuery(setOfInChIs, parentFolder, annoFileAll, annoFileAllPresent)
