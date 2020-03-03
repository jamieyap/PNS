library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.breakfree.cc1.input_data <- Sys.getenv("path.breakfree.cc1.input_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.breakfree.code, "breakfree-data-manip-utils.R"))
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

# Participant IDs for data collected using CC1 platform
ids.cc1 <- list.files(path = path.breakfree.cc1.input_data)

# -----------------------------------------------------------------------------
# Read CC1 random EMA raw data
# -----------------------------------------------------------------------------
list.df.raw.random.cc1 <- list()
ids.nodat.random.cc1 <- NULL

for(i in 1:length(ids.cc1)){
  
  # DO: For all participant IDs listed in ids.cc1
  this.id <- ids.cc1[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc1.input_data, this.id))
  # Pick out file name corresponding to random EMA data
  idx <- grepl("EMA\\+RANDOM\\_EMA\\+PHONE\\+processed.csv", all.files)
  this.file <- all.files[idx]
  
  if(length(this.file)==0){
    # Scenario when a random EMA files does not exist
    ids.nodat.random.cc1 <- c(ids.nodat.random.cc1, this.id) 
  }else{
    # Read file corresponding to random EMA data
    df.raw <- read.csv(file.path(path.breakfree.cc1.input_data, 
                                 this.id, 
                                 this.file), 
                       header = TRUE)
    # Add column to record participant ID
    df.raw$participant.id <- this.id
    
    # Append additional info
    list.df.raw.random.cc1 <- append(list.df.raw.random.cc1, list(df.raw))
  }
}

remove(df.raw, this.id, all.files, this.file)

