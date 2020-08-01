###############################################################################
# ABOUT:
# * Merge data from get-ema-data-frames-by-type.R
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.pns.code, "get-ema-data-frames-by-type.R"))

#------------------------------------------------------------------------------
# Prepare to merge data:
# Change column names in each data frame within list.all to new column names 
# in ema.item.names
#------------------------------------------------------------------------------

# Item names from all EMA types
ema.item.names <- read.csv(file.path(path.pns.output_data, "ema_item_names.csv"), header = TRUE, stringsAsFactors = FALSE)

for(k in 1:length(list.all)){
  current.assessment.type <- names(list.all)[k]
  df.processed <- list.all[[current.assessment.type]]
  old.column.names <- colnames(df.processed)
  these.old <- ema.item.names %>% filter(assessment.type==current.assessment.type) %>% extract2("name.codebook")
  these.new <- ema.item.names %>% filter(assessment.type==current.assessment.type) %>% extract2("name.new")
  
  for(i in 1:length(old.column.names)){
    if(old.column.names[i] %in% these.old){  # if TRUE, then old.column.names[i] corresponds to an EMA item
      idx <- which(these.old==old.column.names[i])  # search for the new column name based on the dictionary of old and new column names
      colnames(df.processed)[i] <- these.new[idx]  # rename ith column of df.processed
    }else{
      next  # do NOT rename ith column of df.processed
    }
  }
  # Update df.processed with new column names
  list.all[[current.assessment.type]] <- df.processed
}


#------------------------------------------------------------------------------
# Transform list.all from a list into a data frame
#------------------------------------------------------------------------------

ema.item.names <- read.csv(file.path(path.pns.output_data, "ema_item_names.csv"), header = TRUE, stringsAsFactors = FALSE)
collect.reshaped.df <- list()
all.names <- ema.item.names$name.new

for(i in 1:length(list.all)){
  current.df <- list.all[[i]]
  
  # Which names in all.names are not current.df column names
  add.these.colnames <- setdiff(all.names, colnames(current.df))
  current.df[,add.these.colnames] <- NA
  
  # Reorder columns
  current.df <- current.df %>% 
    select(id, callnumr, 
           start_study_hrts, quit_hrts, end_study_hrts, 
           start_study_unixts, quit_unixts, end_study_unixts,
           sensitivity,
           record_id, assessment_type, 
           use_as_postquit, with_any_response,
           delivered_hrts, begin_hrts, end_hrts, time_hrts,
           delivered_unixts, begin_unixts, end_unixts, time_unixts,
           all.names)
  
  # Collect reshaped data frame
  collect.reshaped.df <- append(collect.reshaped.df, list(current.df))
}

reshaped.df <- do.call(rbind, collect.reshaped.df)

#------------------------------------------------------------------------------
# Merge Smoking Outcome data frame with EMA data frame
#------------------------------------------------------------------------------
df.smoking <- read.csv(file.path(path.pns.output_data, "smoking_outcome.csv"), stringsAsFactors = FALSE)
colnames.merged <- c(colnames(df.smoking), colnames(reshaped.df))
colnames.merged <- unique(colnames.merged)

# Append column names to df.smoking
# Use setdiff() to identify column names which are in colnames.merged but are not in df.smoking
df.smoking.add.these.colnames <- setdiff(colnames.merged, colnames(df.smoking))
df.smoking[,df.smoking.add.these.colnames] <- NA

# Append column names to df.merged
# Use setdiff() to identify column names which are in colnames.merged but are not in reshaped.df
reshaped.df.add.these.colnames <- setdiff(colnames.merged, colnames(reshaped.df))
reshaped.df[,reshaped.df.add.these.colnames] <- NA

# Merge both data frames
df.smoking <- df.smoking[, colnames.merged]
reshaped.df <- reshaped.df[, colnames.merged]
BIG.df <- rbind(df.smoking, reshaped.df)

#------------------------------------------------------------------------------
# Save merged dataset to csv file
#------------------------------------------------------------------------------
write.csv(BIG.df, file.path(path.pns.output_data, "merged.csv"), row.names = FALSE, na="")

