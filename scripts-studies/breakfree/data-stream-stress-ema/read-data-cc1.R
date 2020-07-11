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
list.ids.backup <- list()
list.ids.none <- list()

# -----------------------------------------------------------------------------
# Read CC1 stress EMA raw data: DATA file
# -----------------------------------------------------------------------------
list.df.raw <- list()

for(i in 1:length(ids.cc1)){
  this.id <- ids.cc1[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc1.input_data, this.id))
  # Pick out file names related to data stream of interest
  idx <- grepl(pattern = "EMA+STRESS_EMA+PHONE+processed.csv", 
               x = all.files, 
               fixed = TRUE)
  # Pick out corresponding files
  this.file <- all.files[idx]
  len <- length(this.file)
  
  # Check other file name
  idx2 <- grepl(pattern = "EMA+STRESS_EMA+PHONE.csv.zip", 
                x = all.files, 
                fixed = TRUE)
  # Pick out corresponding files
  this.file2 <- all.files[idx2]
  len2 <- length(this.file2)
  
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
  }else if(len==0 & len2>0){
    list.ids.backup <- append(list.ids.backup, list(this.id))
  }else{
    list.ids.none <- append(list.ids.none, list(this.id))
  }
}

list.df.raw.stress.DATA.cc1 <- list.df.raw
remove(list.df.raw)

ids.backup.stress.cc1 <- unlist(list.ids.backup)
ids.none.stress.cc1 <- unlist(list.ids.none)

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
save(list.df.raw.stress.DATA.cc1, ids.backup.stress.cc1, ids.none.stress.cc1,
     file = file.path(path.breakfree.staged_data, 
                      "raw.stressEMA.cc1.RData"))

