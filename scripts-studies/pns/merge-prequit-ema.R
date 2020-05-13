###############################################################################
# ABOUT:
# * Aggregates output csv files of get-prequit-ema.R script into one file
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")

# collect.all is a list whose elements are data frames corresponding to
# each type of pre quit EMA
collect.all.df <- list()
collect.all.names <- list()

#------------------------------------------------------------------------------
# Pre Quit Random EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "prequit_random.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_random.csv"), header = TRUE, stringsAsFactors = FALSE)

# Change column names in df.processed to column names in ema.item.names
old.column.names <- colnames(df.processed)
these.old <- ema.item.names$name.codebook
these.new <- ema.item.names$name.new

for(i in 1:length(old.column.names)){
  if(old.column.names[i] %in% these.old){
    idx <- which(these.old==old.column.names[i])
    colnames(df.processed)[i] <- these.new[idx]
  }else{
    next
  }
}

collect.all.df <- append(collect.all.df, list(df.processed))
collect.all.names <- append(collect.all.names, list(ema.item.names$name.new))

remove(df.processed, ema.item.names, old.column.names, these.old, these.new)

#------------------------------------------------------------------------------
# Pre Quit Urge EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "prequit_urge.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_urge.csv"), header = TRUE, stringsAsFactors = FALSE)

# Change column names in df.processed to column names in ema.item.names
old.column.names <- colnames(df.processed)
these.old <- ema.item.names$name.codebook
these.new <- ema.item.names$name.new

for(i in 1:length(old.column.names)){
  if(old.column.names[i] %in% these.old){
    idx <- which(these.old==old.column.names[i])
    colnames(df.processed)[i] <- these.new[idx]
  }else{
    next
  }
}

collect.all.df <- append(collect.all.df, list(df.processed))
collect.all.names <- append(collect.all.names, list(ema.item.names$name.new))

remove(df.processed, ema.item.names, old.column.names, these.old, these.new)

#------------------------------------------------------------------------------
# Pre Quit About to Slip Part 1 EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "prequit_about2slip1.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_about2slip1.csv"), header = TRUE, stringsAsFactors = FALSE)

# Change column names in df.processed to column names in ema.item.names
old.column.names <- colnames(df.processed)
these.old <- ema.item.names$name.codebook
these.new <- ema.item.names$name.new

for(i in 1:length(old.column.names)){
  if(old.column.names[i] %in% these.old){
    idx <- which(these.old==old.column.names[i])
    colnames(df.processed)[i] <- these.new[idx]
  }else{
    next
  }
}

collect.all.df <- append(collect.all.df, list(df.processed))
collect.all.names <- append(collect.all.names, list(ema.item.names$name.new))

remove(df.processed, ema.item.names, old.column.names, these.old, these.new)

#------------------------------------------------------------------------------
# Pre Quit About to Slip Part 2 EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "prequit_about2slip2.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_about2slip2.csv"), header = TRUE, stringsAsFactors = FALSE)

# Change column names in df.processed to column names in ema.item.names
old.column.names <- colnames(df.processed)
these.old <- ema.item.names$name.codebook
these.new <- ema.item.names$name.new

for(i in 1:length(old.column.names)){
  if(old.column.names[i] %in% these.old){
    idx <- which(these.old==old.column.names[i])
    colnames(df.processed)[i] <- these.new[idx]
  }else{
    next
  }
}

collect.all.df <- append(collect.all.df, list(df.processed))
collect.all.names <- append(collect.all.names, list(ema.item.names$name.new))

remove(df.processed, ema.item.names, old.column.names, these.old, these.new)

#------------------------------------------------------------------------------
# Transform collect.all from a list into a data frame
#------------------------------------------------------------------------------
all.names <- unlist(collect.all.names)
collect.reshaped.df <- list()

for(i in 1:length(collect.all.df)){
  current.df <- collect.all.df[[i]]
  # Which names in all.names are not current.df column names
  add.these.colnames <- setdiff(all.names, colnames(current.df))
  current.df[,add.these.colnames] <- NA
  
  # Reorder columns
  current.df <- current.df %>% 
    select(id, record.id, assessment.type,
           delivered.hrts, begin.hrts, end.hrts,
           delivered.unixts, begin.unixts, end.unixts,
           with.any.response,
           all.names)
  
  # Transform all columns to character type
  current.df <- apply(current.df, 2, as.character)
  
  # Collect reshaped data frame
  collect.reshaped.df <- append(collect.reshaped.df , list(current.df))
}

reshaped.df <- do.call(rbind, collect.reshaped.df)

#------------------------------------------------------------------------------
# Save merged dataset to csv file
#------------------------------------------------------------------------------
write.csv(reshaped.df, file.path(path.pns.output_data, "merged_prequit_ema.csv"), row.names = FALSE, na="")

