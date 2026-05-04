# Make an HTML map of EcoEvoJobs.net Data in R
This R script fetches data from EcoEvoJobs.net and outputs two HTML files that 
contain interactive maps of that data: "PermFaculty_<date>.html" is a map of Permanant/Faculty 
positions while "PostDocsJobs_<date>.html" is a map of PostDoc positions. If you use this script,
first review the variables in the section toward the top of the script called "VARS". This section 
contains the path where geodata stores GADM files and cutoffs for the review date among other things.
The output HTML files are stored to the working directory. Examples of these HTML files are also 
included in the repository.
