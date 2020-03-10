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
                          DATA = NA,
                          STATUS = NA,
                          SCHEDULER = NA,
                          CONDITION = NA)

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to random EMA data
  idx.DATA <- match(x="EMA_RANDOM--DATA--org.md2k.ema.csv", table=all.files)
  idx.STATUS <- match(x="EMA_RANDOM--STATUS--org.md2k.scheduler.csv.bz2", table=all.files)
  idx.SCHEDULER <- match(x="EMA--RANDOM--org.md2k.scheduler.csv.bz2", table=all.files)
  idx.CONDITION <- match(x="CONDITION--org.md2k.scheduler.csv.bz2", table=all.files)
  # Pick out corresponding files
  this.file.DATA <- all.files[idx.DATA]
  this.file.STATUS <- all.files[idx.STATUS]
  this.file.SCHEDULER <- all.files[idx.SCHEDULER]
  this.file.CONDITION <- all.files[idx.CONDITION]
  
  # Check whether file exists for this given participant
  ##### DATA ##################################################################
  if(!is.na(this.file.DATA)){
    count <- length(this.file.DATA) # Check for duplicates
    df.tabulate <- df.tabulate %>% mutate(DATA=replace(DATA, user.id==this.id, count))
  }else{
    df.tabulate <- df.tabulate %>% mutate(DATA=replace(DATA, user.id==this.id, 0))
  }
  ##### STATUS ################################################################
  if(!is.na(this.file.STATUS)){
    count <- length(this.file.STATUS) # Check for duplicates
    df.tabulate <- df.tabulate %>% mutate(STATUS=replace(STATUS, user.id==this.id, count))
  }else{
    df.tabulate <- df.tabulate %>% mutate(STATUS=replace(STATUS, user.id==this.id, 0))
  }
  ##### SCHEDULER #############################################################
  if(!is.na(this.file.SCHEDULER)){
    count <- length(this.file.SCHEDULER) # Check for duplicates
    df.tabulate <- df.tabulate %>% mutate(SCHEDULER=replace(SCHEDULER, user.id==this.id, count))
  }else{
    df.tabulate <- df.tabulate %>% mutate(SCHEDULER=replace(SCHEDULER, user.id==this.id, 0)) 
  }
  ##### CONDITION #############################################################
  if(!is.na(this.file.CONDITION)){
    count <- length(this.file.CONDITION) # Check for duplicates
    df.tabulate <- df.tabulate %>% mutate(CONDITION=replace(CONDITION, user.id==this.id, count))
  }else{
    df.tabulate <- df.tabulate %>% mutate(CONDITION=replace(CONDITION, user.id==this.id, 0))
  }
}

write.csv(df.tabulate,
          file.path(path.breakfree.staged_data, 
                    "df.tabulate.random.cc2.csv"), 
          row.names = FALSE)

# -----------------------------------------------------------------------------
# Read CC2 random EMA raw data: DATA file
# -----------------------------------------------------------------------------
list.df.raw.random.DATA.cc2 <- list()

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to random EMA data
  idx.DATA <- match(x="EMA_RANDOM--DATA--org.md2k.ema.csv", table=all.files)
  # Pick out corresponding files
  this.file.DATA <- all.files[idx.DATA]
  
  # Read file if it exists for given participant
  ##### DATA ##################################################################
  if(!is.na(this.file.DATA)){
    df.raw <- read.csv(file.path(path.breakfree.cc2.input_data, 
                                 this.id, 
                                 this.file.DATA), 
                       header = TRUE)
    # Add column to record participant ID
    df.raw$user.id <- this.id
    # Add df.raw to collection
    list.df.raw.random.DATA.cc2 <- append(list.df.raw.random.DATA.cc2, list(df.raw))
  }else{
    next
  }
}

# -----------------------------------------------------------------------------
# Read CC2 random EMA raw data: STATUS file
# -----------------------------------------------------------------------------
list.df.raw.random.STATUS.cc2 <- list()

for(i in 1:length(ids.cc2)){
  this.id <- ids.cc2[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc2.input_data, this.id))
  # Pick out file names related to random EMA data
  idx.STATUS <- match(x="EMA_RANDOM--STATUS--org.md2k.scheduler.csv.bz2", table=all.files)
  # Pick out corresponding files
  this.file.STATUS <- all.files[idx.STATUS]
  
  # Read file if it exists for given participant
  ##### STATUS ##################################################################
  if(!is.na(this.file.STATUS)){
    df.raw <- read.csv(file.path(path.breakfree.cc2.input_data, 
                                 this.id, 
                                 this.file.STATUS), 
                       header = FALSE)  # STATUS files do not contain column names
    # Add column to record participant ID
    df.raw$user.id <- this.id
    # Add df.raw to collection
    list.df.raw.random.STATUS.cc2 <- append(list.df.raw.random.STATUS.cc2, list(df.raw))
  }else{
    next
  }
}

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
save(list.df.raw.random.DATA.cc2, 
     list.df.raw.random.STATUS.cc2,
     file = file.path(path.breakfree.staged_data, 
                      "raw.randomEMA.cc2.RData"))

