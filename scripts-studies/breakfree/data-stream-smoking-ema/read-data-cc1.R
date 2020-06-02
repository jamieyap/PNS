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

# Participant IDs for data collected using CC1 platform
ids.cc1 <- list.files(path = path.breakfree.cc1.input_data)

# -----------------------------------------------------------------------------
# Read CC1 smoking EMA raw data: DATA file
# -----------------------------------------------------------------------------
list.df.raw <- list()

for(i in 1:length(ids.cc1)){
  this.id <- ids.cc1[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc1.input_data, this.id))
  # Pick out file names related to data stream of interest
  idx <- grepl(pattern = "EMA+SMOKING_EMA+PHONE+processed.csv", 
               x = all.files, 
               fixed = TRUE)
  # Pick out corresponding files
  this.file <- all.files[idx]
  len <- length(this.file)
  
  # Check whether file exists for this given participant
  if(len>0){
    df.raw <- read.csv(file.path(path.breakfree.cc1.input_data, 
                                 this.id, 
                                 this.file), 
                       header = TRUE)  # DATA file contains column names
    # Add column to record participant ID
    df.raw <- df.raw %>% mutate(user.id = this.id) %>% select(user.id, everything())
    # Add df.raw to collection
    list.df.raw <- append(list.df.raw, list(df.raw))
  }else{
    next
  }
}

list.df.raw.smoking.DATA.cc1 <- list.df.raw
remove(list.df.raw)

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
save(list.df.raw.smoking.DATA.cc1, 
     file = file.path(path.breakfree.staged_data, 
                      "raw.smokingEMA.cc1.RData"))

