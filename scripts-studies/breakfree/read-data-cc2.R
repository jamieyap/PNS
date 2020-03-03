library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.breakfree.cc2.input_data <- Sys.getenv("path.breakfree.cc2.input_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.breakfree.code, "breakfree-data-manip-utils.R"))
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

# Participant IDs for data collected using CC2 platform
ids.cc2 <- list.files(path = path.breakfree.cc2.input_data)
ids.cc2 <- ids.cc2[grepl("aa_", ids.cc2)]
ids.cc2 <- ids.cc2[!grepl("test", ids.cc2)]

# -----------------------------------------------------------------------------
# Read CC2 random EMA raw data
# -----------------------------------------------------------------------------
list.df.raw.random.cc2 <- list()
ids.nodat.random.cc2 <- NULL

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file name corresponding to random EMA data
  idx <- grepl("EMA\\_RANDOM\\-\\-DATA\\-\\-org.md2k.ema.csv", all.files)
  this.file <- all.files[idx]
  
  if(length(this.file)==0){
    # Scenario when a random EMA files does not exist
    ids.nodat.random.cc2 <- c(ids.nodat.random.cc2, this.id) 
  }else{
    this.file <- this.file[1]
    # Read file corresponding to random EMA data
    df.raw <- read.csv(file.path(path.breakfree.cc2.input_data, 
                                 this.id, 
                                 this.file), 
                       header = TRUE)
    # Add column to record participant ID
    df.raw$participant.id <- this.id
    # Add df.raw to collection
    list.df.raw.random.cc2 <- append(list.df.raw.random.cc2, list(df.raw))
  }
}

remove(df.raw, this.id, all.files, this.file)

