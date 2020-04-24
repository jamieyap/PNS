# About this scipt: 
# Read in raw data from CC2 participants, combine into a list, 
# and save into an R object for further use

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.breakfree.cc1.input_data <- Sys.getenv("path.breakfree.cc1.input_data")
path.breakfree.cc2.input_data <- Sys.getenv("path.breakfree.cc2.input_data")
path.breakfree.staged_data <- Sys.getenv("path.breakfree.staged_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))

# Participant IDs for data collected using CC2 platform
ids.cc2 <- list.files(path = path.breakfree.cc2.input_data)
ids.cc2 <- ids.cc2[grepl("aa_", ids.cc2)]
ids.cc2 <- ids.cc2[!grepl("test", ids.cc2)]

# -----------------------------------------------------------------------------
# Read raw data
# -----------------------------------------------------------------------------
list.df.raw <- list()

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to data stream of interest
  idx <- match(x="INCENTIVE--org.md2k.scheduler.csv.bz2", table=all.files)
  # Pick out corresponding files
  this.file <- all.files[idx]
  
  # Check whether file exists for this given participant
  if(!is.na(this.file)){
    df.raw <- read.csv(file.path(path.breakfree.cc2.input_data, 
                                 this.id, 
                                 this.file), 
                       header = FALSE)
    # Add column to record participant ID
    df.raw <- df.raw %>% mutate(user.id = this.id) %>% select(user.id, everything())
    # Add df.raw to collection
    list.df.raw <- append(list.df.raw, list(df.raw))
  }else{
    next
  }
}

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
list.df.raw.incentive.cc2 <- list.df.raw
remove(list.df.raw)

save(list.df.raw.incentive.cc2, file = file.path(path.breakfree.staged_data, "raw.incentive.cc2.RData"))
