# About this script: In this script, we set up puff marker data from the 
# Break Free study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
folder.puffmarker_data <- Sys.getenv("folder.puffmarker_data")
folder.puffmarker_unzipped <- Sys.getenv("folder.puffmarker_unzipped")
this.study <- "BreakFree"
this.info <- "PUFFMARKER\\_SMOKING\\_EPISODE\\+PHONE" # timing of beginning of smoking episodes
source(file.path(path.code, "puffmarker-utils.R"))

#------------------------------------------------------------------------------
# Read data on this.info
#------------------------------------------------------------------------------

puffmarker.zipfile.names <- list.files(file.path(path.input_data, this.study, folder.puffmarker_data))

start.time <- Sys.time()
list.df <- lapply(X = puffmarker.zipfile.names, FUN = ExtractThisInfo)
end.time <- Sys.time()

df <- bind_rows(list.df)

#------------------------------------------------------------------------------
# Write data
#------------------------------------------------------------------------------
write.csv(df, file.path(path.input_data, this.study, folder.puffmarker_unzipped, "puffmarker.smoking.episodes.csv"), row.names = FALSE)

