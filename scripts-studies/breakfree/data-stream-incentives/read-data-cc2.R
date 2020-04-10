# About this scipt: 
# Read in raw data from CC2 participants, combine into a list,
# and save into an R object for further use

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

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
# Keep tabs: which participants do not have certain files
# -----------------------------------------------------------------------------
df.tabulate <- data.frame(user.id = ids.cc2,
                          INCENTIVE = NA)

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to random EMA data
  idx.INCENTIVE <- match(x="INCENTIVE--org.md2k.scheduler.csv.bz2", table=all.files)
  # Pick out corresponding files
  this.file <- all.files[idx.INCENTIVE]
  
  # Check whether file exists for this given participant
  ##### INCENTIVE #############################################################
  if(!is.na(this.file)){
    count <- length(this.file) # Check for duplicates
    df.tabulate <- df.tabulate %>% mutate(INCENTIVE=replace(INCENTIVE, user.id==this.id, count))
  }else{
    df.tabulate <- df.tabulate %>% mutate(INCENTIVE=replace(INCENTIVE, user.id==this.id, 0))
  }
}

write.csv(df.tabulate,
          file.path(path.breakfree.staged_data, 
                    "df.tabulate.incentive.cc2.csv"), 
          row.names = FALSE)

# -----------------------------------------------------------------------------
# Read CC2 incentive raw data
# -----------------------------------------------------------------------------
list.df.raw.incentive.cc2 <- list()

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to random EMA data
  idx.INCENTIVE <- match(x="INCENTIVE--org.md2k.scheduler.csv.bz2", table=all.files)
  # Pick out corresponding files
  this.file <- all.files[idx.INCENTIVE]
  
  # Check whether file exists for this given participant
  ##### INCENTIVE #############################################################
  if(!is.na(this.file)){
    df.raw <- read.csv(file.path(path.breakfree.cc2.input_data, 
                                 this.id, 
                                 this.file), 
                       header = FALSE)
    # Add column to record participant ID
    df.raw$user.id <- this.id
    # Add df.raw to collection
    list.df.raw.incentive.cc2 <- append(list.df.raw.incentive.cc2, list(df.raw))
  }else{
    next
  }
}

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
save(list.df.raw.incentive.cc2,
     file = file.path(path.breakfree.staged_data, 
                      "raw.incentive.cc2.RData"))

