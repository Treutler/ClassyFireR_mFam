
## files and folders
parentFolder <- "/home/htreutle/Downloads/MetSWATH/MONA/ClassyFire"
annoFileAll        <- ""
annoFileAllPresent <- "/home/htreutle/Downloads/MetSWATH/MONA/181019_MSMS_merge_HR_scaffolds.tsv"

setOfInChIs <- readLines(con = "/home/htreutle/Downloads/MetSWATH/MONA/190522_AllInchis_HR.txt")

setOfInChIsNotClassyFireable <- classyFireQuery(setOfInChIs, parentFolder, annoFileAll, annoFileAllPresent)
