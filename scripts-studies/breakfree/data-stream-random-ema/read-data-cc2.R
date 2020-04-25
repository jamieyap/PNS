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
# Read CC2 random EMA raw data: DATA file
# -----------------------------------------------------------------------------
list.df.raw <- list()

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to data stream of interest
  idx <- match(x="EMA_RANDOM--DATA--org.md2k.ema.csv", table=all.files)
  # Pick out corresponding files
  this.file <- all.files[idx]
  
  # Read file if it exists for given participant
  if(!is.na(this.file)){
    df.raw <- read.csv(file.path(path.breakfree.cc2.input_data, 
                                 this.id, 
                                 this.file), 
                       header = TRUE) # DATA files contain column names
    # Add column to record participant ID
    df.raw <- df.raw %>% mutate(user.id = this.id) %>% select(user.id, everything())
    # Add df.raw to collection
    list.df.raw <- append(list.df.raw, list(df.raw))
  }else{
    next
  }
}

list.df.raw.random.DATA.cc2 <- list.df.raw
remove(list.df.raw, all.files)

# -----------------------------------------------------------------------------
# Read CC2 random EMA raw data: STATUS file
# -----------------------------------------------------------------------------
list.df.raw <- list()

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to data stream of interest
  idx <- match(x="EMA_RANDOM--STATUS--org.md2k.scheduler.csv.bz2", table=all.files)
  # Pick out corresponding files
  this.file <- all.files[idx]
  
  # Read file if it exists for given participant
  if(!is.na(this.file)){
    df.raw <- read.csv(file.path(path.breakfree.cc2.input_data, 
                                 this.id, 
                                 this.file), 
                       header = FALSE)  # STATUS files do not contain column names
    # Add column to record participant ID
    df.raw <- df.raw %>% mutate(user.id = this.id) %>% select(user.id, everything())
    # Add df.raw to collection
    list.df.raw <- append(list.df.raw, list(df.raw))
  }else{
    next
  }
}

list.df.raw.random.STATUS.cc2 <- list.df.raw
remove(list.df.raw, all.files)

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
save(list.df.raw.random.DATA.cc2, 
     list.df.raw.random.STATUS.cc2,
     file = file.path(path.breakfree.staged_data, 
                      "raw.randomEMA.cc2.RData"))

