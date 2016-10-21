#234567890#234567890#234567890#234567890#234567890#234567890#234567890#234567890 (80 chars)
# Read in a blm.csv file -- a Binary Link Matrix (BLM)
# (https://github.com/steve9000gi/binary-link-matrix) that
# represents the connectivity between nodes in a System Support Map (SSM)
# (http://syssci.renci.org/ssm/). Then read in a sortedList.json file
# (https://github.com/steve9000gi/sort), which contains node names from a
# directory of SSMs, grouped by "codes" by an individual using the sort web page
# (http://syssci.renci.org/sort/). For each node in the BLM, find the code with 
# which it's associated and add that code to a new column. Output a Coded BLM
# (CBLM) file which is identical to the BLM file that was originally read in,
# except with the addition of the new column of codes.
#
# Usage: RScript /path/to/AddCodesToBLM.R SortInfile BLMInfile CBLMoutfile
#

library(methods)
library(tools)
library(jsonlite)

args = commandArgs()
inputSortFileName = args[6]
inputBLMFileName = args[7]
outputCBLMFileName = args[8]

print(paste("inputBLMFileName: ", inputBLMFileName, "; outputCBLMFileName: ",
      outputCBLMFileName))

sortedList = fromJSON(inputSortFileName)
options(stringsAsFactors=FALSE)
blm = read.csv(inputBLMFileName, header = TRUE, sep = "\t", quote = "");

n = names(sortedList)

for (i in 1:length(sortedList)) {
  code = n[i]
  l = as.list(sortedList[code][[1]])
  for (j in 1:length(l)) {
    print(paste(code, ": ", l[j]), sep = "")
  }
} 

for (i in 1:length(blm[,4])) {
  print(paste(i, blm[i,4]))
}
