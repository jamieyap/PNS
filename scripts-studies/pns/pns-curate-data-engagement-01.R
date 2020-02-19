# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the PNS study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"data-manip-utils.R"))
source(file.path(path.code,"pns-run-curation/pns-get-items.R"))

engagement.df <- left_join(pns.quit.dates, variables.df, by = "id")

write.csv(engagement.df, file.path(path.output_data, "PNS/df.analysis.engagement.csv"), row.names = FALSE)
