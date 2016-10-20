<h3>Add Codes to BLM,/h3>

<p>Read in a blm.csv file -- a Binary Link Matrix (BLM)
(https://github.com/steve9000gi/binary-link-matrix) that
represents the connectivity between nodes in a System Support Map (SSM)
(http://syssci.renci.org/ssm/). Then read in a sortedList.json file
(https://github.com/steve9000gi/sort), which contains node names from a
directory of SSMs, grouped by "codes" by an individual using the sort web page
(http://syssci.renci.org/sort/). For each node in the BLM, find the code with 
which it's associated and add that code to a new column. Output a Coded BLM
(CBLM) file which is identical to the BLM file that was originally read in,
except with the addition of the new column of codes.</p>

<p>Usage: RScript /path/to/AddCodesToBLM.R SortInfile BLMInfile CBLMoutfile</p>
