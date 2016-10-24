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

# In order to access each code as the value associated with its node name, build
# an "inverse list," where the key is the node name, and the value is the code.
buildInverseList = function(inputSortFileName) {
  sortedList = fromJSON(inputSortFileName)
  n = names(sortedList) # these are the codes
  inverseList = list() 
  for (i in 1:length(sortedList)) {
    code = n[i]
    l = as.list(sortedList[code][[1]])
    for (j in 1:length(l)) {
      inverseList[l[[j]]] = code;
    }
  } 
  return (inverseList)
}

# Build a list of codes with each element in the same position as its
# corresponding node name row in the input BLM.
buildOrderedCodeList = function(blm, inverseList) {
  nodeNameColNum = 4 # the column number for node names in the input BLM
  codes = c("Code") # For our purposes each column header is just another string
  for (i in 2:length(blm[,nodeNameColNum])) {
    codes[[i]] = inverseList[[blm[i,nodeNameColNum]]]
  }
  return (codes)
}

# Add the ith column to be written to file as the ith element in list "outList".
buildOutList = function(blm, orderedCodeList) {
  nColsOut = dim(blm)[2] + 1
  nColsBefore = 4
  nColsAfter = dim(blm)[2] - nColsBefore
  codeColIx = nColsBefore + 1
  outList <- vector("list", nColsOut)
  for (i in 1:nColsBefore) { # Copy the preceding columns from the BLM
    outList[[i]] <- blm[,i]
  }
  outList[[codeColIx]] = orderedCodeList # Insert the new column of codes
  for (i in (codeColIx + 1):nColsOut) {  # Copy remaining columns from the BLM
    outList[[i]] <- blm[,(i - 1)]
  }
  devNull = do.call(cbind, outList) 
  return (outList)
}

# main:

args = commandArgs()
inputSortFileName = args[6]
inputBLMFileName = args[7]
outputCBLMFileName = args[8]

options(stringsAsFactors = FALSE)
blm = read.csv(inputBLMFileName, header = FALSE, sep = "\t", quote = "");

inverseList = buildInverseList(inputSortFileName)
orderedCodeList = buildOrderedCodeList(blm, inverseList)
outList = buildOutList(blm, orderedCodeList)

write.table(outList, 
            file = outputCBLMFileName,
            append = FALSE, 
            quote = FALSE, 
            sep = "\t", 
            row.names = FALSE, 
            col.names = FALSE,
            na = "")
