
# Script for the query of ClassyFire queries

## Install

Install in R console as follows.
```
install.packages("stringr")
install.packages("stringi")
```

## Run the script

```
source("/mnt/ifs/data/IPB/Projects/2017_005_MS-databases/mFam contributions/mFam Aggregation/ClassyFireR_mFam/ClassyFireRequests.R")

## files and folders
parentFolder <- "/home/htreutle/Downloads/MetSWATH/MONA/ClassyFire"
annoFileAll        <- ""
annoFileAllPresent <- "/home/htreutle/Downloads/MetSWATH/MONA/181019_MSMS_merge_HR_scaffolds.tsv"

setOfInChIs <- readLines(con = "/home/htreutle/Downloads/MetSWATH/MONA/190522_AllInchis_HR.txt")

setOfInChIsNotClassyFireable <- classyFireQuery(setOfInChIs, parentFolder, annoFileAll, annoFileAllPresent)

```
