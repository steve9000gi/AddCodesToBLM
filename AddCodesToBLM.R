# Read in a directory of *BLM.csv files. Each Binary Link Matrix (BLM)
# (https://github.com/steve9000gi/binary-link-matrix) represents the
# connectivity between nodes in a System Support Map (SSM)
# (http://syssci.renci.org/ssm/). Also read in a SortedItems.json file
# (https://github.com/steve9000gi/sort), which contains node names from a
# directory of SSMs, grouped by "codes" by a human being using the sort web page
# (http://syssci.renci.org/sort/).
#
# For each BLM file in path/to/BLMInputDir:
#   1) For each node in that BLM, find the code with which that node is
#      associated (in the SortedItems.json input file, the keys are "codes" and 
#      the values are "node names") and add that code to a new column;
#   2) Output a Coded BLM (CBLM) file to /path/to/CBLMOutputDir which is
#      identical to its corresponding BLM file, except with the addition of
#      the new column of codes.
#
# Usage: RScript /path/to/AddCodesToBLM.R 
#                /path/to/SortedItems.json
#                /path/to/BLMInputDir
#                /path/to/CBLMOutputDir
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
#
# NOTE: This function handles the new unified JSON file format as used by blm.R
# and the sort website.
buildInvertingLists = function(inputSortFilePath) {
  json = fromJSON(inputSortFilePath,
                 simplifyVector = FALSE,
                 simplifyDataFrame = TRUE)
  sorted = json$sorted
  codes = sorted$title
  orderedNameList = list()
  orderedValueList = list()
  orderedListCount = 1
  numCodes = length(codes)
  for (i in 1:numCodes) { # for each code
    code = codes[i]
    vals = as.list(sorted$textItems[[i]][[1]]) # values for current code
    numVals = length(vals)
    if (numVals == 0) next # Don't try processing an empty list.
    for (j in 1:numVals) { # for each text item belonging to the current code
      orderedNameList[orderedListCount] = code;
      orderedValueList[orderedListCount] = vals[[j]]
      orderedListCount = orderedListCount + 1
    }
  }
  return (list(orderedNameList, orderedValueList))
}

# Keeping around the version that handles the old, simpler JSON format.
buildInvertingListsOld = function(inputSortFilePath) {
  sortOutputList = fromJSON(inputSortFilePath)
  sortedList = sortOutputList$sorted
  n = names(sortedList) # these are the codes
  orderedNameList = list()
  orderedValueList = list()
  orderedListCount = 1
  #write(paste("length(sortedList):", length(sortedList)), stdout())
  for (i in 1:length(sortedList)) {
    code = n[i]
    vals = as.list(sortedList[code][[1]]) # list of values for current code
    numVals = length(vals)
    #print(vals, row.names = FALSE)
    if (numVals == 0) next # Don't try processing empty list
    for (j in 1:length(vals)) {
      #write(paste(i, ", ", j, sep = ""), stdout()) 
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
#       write(paste(blm[i,nodeNameColNum], " =?= ", nodeNames[j], ": ", (blm[i,nodeNameColNum] == nodeNames[j]), sep = ""), stderr())

      if (blm[i,nodeNameColNum] == nodeNames[j]) {
        codesOut[i] = codes[j]
        found = TRUE
        break
      }
    }
    if (found) {
      found = FALSE
    } else {
      write(paste("No code for '", blm[i,nodeNameColNum], "'", sep = ""),
            stderr())
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

makeCLBMFile = function(invertingLists, inputBLMFilePath, outputCBLMFilePath) {
  options(stringsAsFactors = FALSE)
  blm = read.csv(inputBLMFilePath, header = FALSE, sep = "\t", quote = "");
  orderedCodeList = buildOrderedCodeList(blm, invertingLists)
  cblm = buildCBLM(blm, orderedCodeList)
  write.table(cblm,
	      file = outputCBLMFilePath,
	      append = FALSE,
	      quote = FALSE,
	      sep = "\t",
	      row.names = FALSE,
	      col.names = FALSE,
	      na = "")
}

# The CBLM file gets the path to the CBLM output directory plus the same name
# as the BLM input file except there's a 'C' inserted just before "BLM.csv".
# Assumes that the input file name ends with "-BLM.csv".
generateOutputCBLMFilePath = function(inputFilePath, outputDirectoryPath) {
  inputFileName = strsplit(inputFilePath, "/")[[1]] # Remove path, if any.
  inFNameSplit = strsplit(inputFileName, "-BLM")
  outputCBLMFilePath = paste0(outputDirectoryPath, "/", inFNameSplit[[1]][1],
                              "-CBLM.csv", collapse = "")
  return (outputCBLMFilePath)
}


# main:

args = commandArgs()
inputSortFilePath = args[6]
inputBLMDir = args[7]
outputCBLMDir = args[8]

invertingLists = buildInvertingLists(inputSortFilePath)

setwd(inputBLMDir)
inputFiles = list.files(path = inputBLMDir, pattern = "*-BLM.csv")
dir.create(outputCBLMDir, showWarnings = FALSE)
nInputFiles = length(inputFiles)
write(paste("Number of BLM input files:", nInputFiles), stdout())

for (i in 1:nInputFiles) {
  outputCBLMFilePath = generateOutputCBLMFilePath(inputFiles[i], outputCBLMDir)
  write(paste(i, ": ", inputFiles[i], " -> ", outputCBLMFilePath, sep = ""),
        stdout()) 
  makeCLBMFile(invertingLists, inputFiles[i], outputCBLMFilePath)
}

