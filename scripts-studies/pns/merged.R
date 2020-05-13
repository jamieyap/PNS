###############################################################################
# ABOUT:
# * Aggregates output csv files of get-postquit-ema.R and get-prequit-ema.R 
# with get-poutcome-smoking.R scripts into one file
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
# each type of post quit EMA
collect.all.df <- list()
collect.all.names <- list()

#------------------------------------------------------------------------------
# Post Quit Random EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "postquit_random.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_random.csv"), header = TRUE, stringsAsFactors = FALSE)

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
# Post Quit Urge EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "postquit_urge.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_urge.csv"), header = TRUE, stringsAsFactors = FALSE)

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
# Post Quit About to Slip Part One EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "postquit_abouttoslippartone.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_abouttoslippartone.csv"), header = TRUE, stringsAsFactors = FALSE)

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
# Post Quit About to Slip Part Two EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "postquit_abouttoslipparttwo.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_abouttoslipparttwo.csv"), header = TRUE, stringsAsFactors = FALSE)

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
# Post Quit Already Slipped EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "postquit_alreadyslipped.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_postquit_alreadyslipped.csv"), header = TRUE, stringsAsFactors = FALSE)

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
# Pre Quit Smoking Part One EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "prequit_smokingpartone.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_smokingpartone.csv"), header = TRUE, stringsAsFactors = FALSE)

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
# Pre Quit Smoking Part Two EMA
#------------------------------------------------------------------------------
df.processed <- read.csv(file.path(path.pns.output_data, "prequit_smokingparttwo.csv"), stringsAsFactors = FALSE)
ema.item.names <- read.csv(file.path(path.pns.output_data, "item_names_prequit_smokingparttwo.csv"), header = TRUE, stringsAsFactors = FALSE)

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
# Smoking Outcome
#------------------------------------------------------------------------------
df.smoking <- read.csv(file.path(path.pns.output_data, "smoking.csv"), stringsAsFactors = FALSE)
cols.df.smoking <- colnames(df.smoking)
add.these.colnames <- c("record.id", "assessment.type",
                        "delivered.hrts", "begin.hrts", "end.hrts",
                        "delivered.unixts", "begin.unixts", "end.unixts",
                        "with.any.response", setdiff(all.names, cols.df.smoking))
df.smoking[,add.these.colnames] <- NA

# Reorder columns in df.smoking
df.smoking <- df.smoking %>% select(cols.df.smoking, add.these.colnames)

#------------------------------------------------------------------------------
# Merge Smoking Outcome data frame with EMA data frame
#------------------------------------------------------------------------------
cols.df.smoking <- cols.df.smoking[cols.df.smoking!="id"]
reshaped.df <- as.data.frame(reshaped.df)
reshaped.df[,cols.df.smoking] <- NA
reshaped.df <- reshaped.df %>% select(id, cols.df.smoking, add.these.colnames)

# Merge the two data frames
bigdf <- rbind(df.smoking, reshaped.df)

#------------------------------------------------------------------------------
# Save merged dataset to csv file
#------------------------------------------------------------------------------
write.csv(bigdf, file.path(path.pns.output_data, "merged.csv"), row.names = FALSE, na="")

