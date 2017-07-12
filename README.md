<h3>Add Codes to BLM</h3>

<p>Read in a directory of BLM.csv files. Each Binary Link Matrix (BLM)
(https://github.com/steve9000gi/binary-link-matrix) represents the
connectivity between nodes in a System Support Map (SSM)
(http://syssci.renci.org/ssm/). Also read in a SortedItems.json file
(https://github.com/steve9000gi/sort), which contains node names from a
directory of SSMs, grouped by "codes" by a human being using the sort web page
(http://syssci.renci.org/sort/).</p>

<p>For each BLM file in path/to/BLMInputDir:</p>

<ol>
  <li>For each node in that BLM, find the code with which that node is
      associated (in the SortedItems.json input file, the keys are "codes" and 
      the values are "node names") and add that code to a new column;</li>
  <li>Output a Coded BLM (CBLM) file to /path/to/CBLMOutputDir which is
      identical to its corresponding BLM file, except with the addition of
      the new column of codes.</li>
</ol>

<p>Usage: RScript /path/to/AddCodesToBLM.R 
               /path/to/SortedItems.json
               /path/to/BLMInputDir
               /path/to/CBLMOutputDir
</p>
