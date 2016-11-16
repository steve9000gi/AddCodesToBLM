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

# Return a list comprised of two other lists: one is a list of the keys from
# the input sort (.json) file, the other is a list of the values from that file.
# We expect the keys to be "codes," i.e., the names of categories that have been
# created by the sorting process, and that each of those categories may include
# multiple values. As a consequence, the list of names may have duplicate
# entries, because the intent of this exercise is that the index for each value
# in the list of values will be the same as the index for its corresponding name
# in the list of names. Example:
# 
# sortedList:
# {
#   "a a": ["a 1", "a 2"],
#   "b b": ["b 1", "b 2", "b 3"]
# }
# orderedNameList: ("a a", "a a", "b b", "b b", "b b")
# orderedValueList: ("a 1", "a 2", "b 1", "b 2", "b 3")
#
# Thus, if you want to find the name ("code") that corresponds to, say, value
# "b 2", then find the index of "b 2" in orderedValueList (4) and look at the
# corresponding (i.e., 4th) element in orderedNameList, and you have it: "b b".
#
# An inverted associative array would be nice, except that both names and values
# are likely to include white space and other characters that are illegal for
# keys in R associative arrays. Yes, you can surround the string with backticks,
# but I've been unsuccessful in doing so programmatically.
buildInvertingLists = function(inputSortFileName) {
  sortedList = fromJSON(inputSortFileName)
  n = names(sortedList) # these are the codes
  orderedNameList = list()
  orderedValueList = list()
  orderedListCount = 1
  for (i in 1:length(sortedList)) {
    code = n[i]
    vals = as.list(sortedList[code][[1]]) # list of values for current code
    for (j in 1:length(vals)) {
      orderedNameList[orderedListCount] = code;
      orderedValueList[orderedListCount] = vals[[j]]
      orderedListCount = orderedListCount + 1
    }
  }
  return (list(orderedNameList, orderedValueList))
}

# Build a list of codes with each element in the same position as its
# corresponding node name row in the input BLM.
buildOrderedCodeList = function(blm, invertingLists) {
  nodeNameColNum = 4 # the column number for node names in the input BLM
  codes = invertingLists[[1]]
  nodeNames = invertingLists[[2]]
  codesOut = list()
  codesOut[1] = "Code" # Column header in the output cblm file.
  for (i in 2:length(blm[,nodeNameColNum])) {
    # Examine each node name until we find a match in nodeNames. The index of
    # that match is j. Append a new codesOut element which is the string at
    # codes[j]. If no match, print a warning and append an empty string.
    found = FALSE
    for (j in 1:length(nodeNames)) { 
      if (blm[i,nodeNameColNum] == nodeNames[j]) {
        codesOut[i] = codes[j]
        found = TRUE
        break
      }
    }
    if (found) {
      found = FALSE
    } else {
      print(paste("Failed to find code for '", blm[i,nodeNameColNum], "'",
            sep = ""))
      codesOut[i] = ""
    }
  }
  return (codesOut)
}

# Return a list with contents identical to those of the input blm except with
# the orderedCodeList inserted as a column immediately following that of the
# node names in the blm.
buildCBLM = function(blm, orderedCodeList) {
  nColsOut = dim(blm)[2] + 1
  nColsBefore = 4
  nColsAfter = dim(blm)[2] - nColsBefore
  codeColIx = nColsBefore + 1
  cblm <- vector("list", nColsOut) # coded binary link matrix
  for (i in 1:nColsBefore) { # Copy the preceding columns from the BLM
    cblm[[i]] <- blm[,i]
  }
  cblm[[codeColIx]] = unlist(orderedCodeList) # Insert new column of codes.
  for (i in (codeColIx + 1):nColsOut) {  # Copy remaining columns from the BLM.
    cblm[[i]] <- blm[,(i - 1)]
  }
  devNull = do.call(cbind, cblm) 
  return (cblm)
}

makeCLBMFile = function(invertingLists, inputBLMFileName, outputCBLMFileName) {
  options(stringsAsFactors = FALSE)
  blm = read.csv(inputBLMFileName, header = FALSE, sep = "\t", quote = "");
  orderedCodeList = buildOrderedCodeList(blm, invertingLists)
  cblm = buildCBLM(blm, orderedCodeList)
  write.table(cblm,
	      file = outputCBLMFileName,
	      append = FALSE,
	      quote = FALSE,
	      sep = "\t",
	      row.names = FALSE,
	      col.names = FALSE,
	      na = "")
}

# main:

args = commandArgs()
inputSortFileName = args[6]
inputBLMFileName = args[7]
outputCBLMFileName = args[8]

invertingLists = buildInvertingLists(inputSortFileName)
makeCLBMFile(invertingLists, inputBLMFileName, outputCBLMFileName)
