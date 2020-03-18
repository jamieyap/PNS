library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.breakfree.cc1.input_data <- Sys.getenv("path.breakfree.cc1.input_data")
path.breakfree.staged_data <- Sys.getenv("path.breakfree.staged_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))

# Participant IDs for data collected using CC1 platform
ids.cc1 <- list.files(path = path.breakfree.cc1.input_data)

# -----------------------------------------------------------------------------
# Keep tabs: which participants do not have certain files
# -----------------------------------------------------------------------------
df.tabulate <- data.frame(user.id = ids.cc1,
                          DATA = NA)

for(i in 1:length(ids.cc1)){
  this.id <- ids.cc1[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc1.input_data, this.id))
  # Pick out file names related to random EMA data
  idx.DATA <- grepl(pattern = "EMA+RANDOM_EMA+PHONE+processed.csv", 
                    x = all.files, 
                    fixed=TRUE)
  # Pick out corresponding files
  this.file.DATA <- all.files[idx.DATA]
  len <- length(this.file.DATA)
  
  # Check whether file exists for this given participant
  ##### DATA ##################################################################
  if(len>0){
    count <- length(this.file.DATA) # Check for duplicates
    df.tabulate <- df.tabulate %>% mutate(DATA=replace(DATA, user.id==this.id, count))
  }else{
    df.tabulate <- df.tabulate %>% mutate(DATA=replace(DATA, user.id==this.id, 0))
  }
}

write.csv(df.tabulate,
          file.path(path.breakfree.staged_data, 
                    "df.tabulate.randomEMA.cc1.csv"), 
          row.names = FALSE)

# -----------------------------------------------------------------------------
# Read CC1 random EMA raw data: DATA file
# -----------------------------------------------------------------------------
list.df.raw.random.DATA.cc1 <- list()

for(i in 1:length(ids.cc1)){
  this.id <- ids.cc1[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc1.input_data, this.id))
  # Pick out file names related to random EMA data
  idx.DATA <- grepl(pattern = "EMA+RANDOM_EMA+PHONE+processed.csv", 
                    x = all.files, 
                    fixed=TRUE)
  # Pick out corresponding files
  this.file.DATA <- all.files[idx.DATA]
  len <- length(this.file.DATA)
  
  # Check whether file exists for this given participant
  ##### DATA ##################################################################
  if(len>0){
    df.raw <- read.csv(file.path(path.breakfree.cc1.input_data, 
                                 this.id, 
                                 this.file.DATA), 
                       header = TRUE)
    # Add column to record participant ID
    df.raw$user.id <- this.id
    
    # Append additional info
    list.df.raw.random.DATA.cc1 <- append(list.df.raw.random.DATA.cc1, list(df.raw))
  }else{
    next
  }
}

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
save(list.df.raw.random.DATA.cc1, 
     file = file.path(path.breakfree.staged_data, 
                      "raw.randomEMA.cc1.RData"))

