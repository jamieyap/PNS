# About this script: In this script, we add variables corresponding to each
# time interval in the datasets for analyses.

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"data-manip-utils.R"))
use.study <- "BreakFree"

#------------------------------------------------------------------------------
# Add variables corresponding to each time interval to datasets for analyses
#------------------------------------------------------------------------------

df.all <- read.csv(file.path(path.output_data,"BreakFree/df.all.csv"), header=TRUE, stringsAsFactors = FALSE)
df.analysis.01 <- read.csv(file.path(path.output_data,"BreakFree/df.analysis.01.csv"), header=TRUE, stringsAsFactors = FALSE)
df.analysis.02a <- read.csv(file.path(path.output_data,"BreakFree/df.analysis.02a.csv"), header=TRUE, stringsAsFactors = FALSE)
df.analysis.02b <- read.csv(file.path(path.output_data,"BreakFree/df.analysis.02b.csv"), header=TRUE, stringsAsFactors = FALSE)

ids.participants <- unique(df.analysis.01$id)
n.participants <- length(ids.participants)
source(file.path(path.code, "construct-variables-intervals.R"))

write.csv(df.analysis.01, file.path(path.output_data,"BreakFree/df.analysis.01.csv"), row.names = FALSE)
write.csv(df.analysis.02a, file.path(path.output_data,"BreakFree/df.analysis.02a.csv"), row.names = FALSE)
write.csv(df.analysis.02b, file.path(path.output_data,"BreakFree/df.analysis.02b.csv"), row.names = FALSE)


