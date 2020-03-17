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
load(file.path(path.breakfree.staged_data, "raw.randomEMA.cc2.RData"))

###############################################################################
# Create a question bank file containing info for each EMA item
###############################################################################
df.raw <- list.df.raw.random.DATA.cc2[[23]]  # Pick one individual

list.items <- list()
all.col.names <- colnames(df.raw)
# There are 65 items in a Random EMA for CC2 participants
# The last item is a thank you message
# Thus, idx ranges from 0 to 63 (numbering begins at 0, not 1)

for(idx in 0:63){  
  these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
  these.col.names <- all.col.names[these.col.names]
  df.item <- df.raw %>% select(these.col.names)  
  df.item <- df.item[1,] # Just grab the first row
  
  # Note: Column names from CC1 to CC2 changed
  # For each of the EMA items, get:
  # question number: questions_idx_question_id
  # question text: questions_idx_question_text
  # question type: questions_idx_question_type
  # options for possible responses: questions_idx_response_option_nn
  idx.id <- match(x = paste("questions_",idx,"_question_id", sep=""), table = these.col.names)
  idx.text <- match(x = paste("questions_",idx,"_question_text", sep=""), table = these.col.names)
  idx.type <- match(x = paste("questions_",idx,"_question_type", sep=""), table = these.col.names)
  idx.options <- grep(pattern = paste("questions_",idx,"_response_option_", sep=""), x = these.col.names)
  
  # Collect info into one data frame
  question.id <- df.item[these.col.names[idx.id]]
  question.text <- df.item[these.col.names[idx.text]]
  question.type <- df.item[these.col.names[idx.type]]
  question.options <- df.item[these.col.names[idx.options]]
  
  question.id <- as.numeric(as.matrix(question.id))
  question.text <- as.character(as.matrix(question.text))
  question.type <- as.character(as.matrix(question.type))
  question.options <- as.character(as.matrix(question.options))
  question.options <- paste(question.options, collapse = "},{")
  question.options <- paste("{", question.options, "}", sep="")
  
  df.collect <- data.frame(question.id = question.id,
                           question.text = question.text,
                           question.type = question.type,
                           question.options = question.options)
  
  list.items <- append(list.items,list(df.collect))
}

items.cc2 <- do.call(rbind, list.items)

# Write out items.cc2 to a csv file
#write.csv(items.cc2, 
#          file.path(path.breakfree.output_data, "random.ema.items.cc2.csv"), 
#          row.names=FALSE)

###############################################################################
# Create an id-variable for each ema in the raw data:
# COMPLETED, ABANDONED_BY_TIMEOUT EMAs
###############################################################################
N <- length(list.df.raw.random.DATA.cc2)

for(i in 1:N){
  df.raw <- list.df.raw.random.DATA.cc2[[i]]
  df.raw$random.ema.id <- 1:nrow(df.raw)
  df.raw$ema.type <- "random"
  list.df.raw.random.DATA.cc2[[i]] <- df.raw
}

###############################################################################
# CHECK: determine the number of EMAs we have thus far per person
###############################################################################
max.ema.count <- lapply(list.df.raw.random.DATA.cc2, function(x){
  df <- data.frame(user.id = as.character(unique(x$user.id)),
                   M = as.numeric(max(x$random.ema.id)))
  df$user.id <- as.character(df$user.id)
  return(df)
})

max.ema.count <- do.call(rbind, max.ema.count)

###############################################################################
# Create an id-variable for each ema in the raw data:
# MISSED EMAs
###############################################################################
# Obtain columns in each person's STATUS file corresponding to MISSED EMAs
list.df.raw.random.STATUS.cc2 <- lapply(list.df.raw.random.STATUS.cc2, function(x){
  
  df <- x %>% 
    select(user.id, V1, V4) %>% 
    mutate(status = as.character(V4),
           prompt.ts = as.character(V1)) %>%
    filter(status=="MISSED") %>%
    mutate(prompt.ts = as.numeric(prompt.ts)) %>%
    select(user.id, prompt.ts, status)
  
  if(nrow(df)==0){
    df <- NULL
  }
  
  return(df)
})

# Remove NULL elements of the list
list.df.raw.random.STATUS.cc2 <- discard(list.df.raw.random.STATUS.cc2, is.null)

# Apply a function to each element of list.df.tmp
list.df.raw.random.STATUS.cc2 <- lapply(list.df.raw.random.STATUS.cc2, function(this.df, counts.df = max.ema.count){
  
  this.user.id <- unique(this.df$user.id)
  M <- counts.df[which(counts.df$user.id == this.user.id), "M"]
  if(length(M)>0){
    this.df$random.ema.id <- ((M+1):(M+nrow(this.df)))
  }else{  # length(M)==0
    this.df$random.ema.id <- (1:nrow(this.df))
  }
  
  this.df$ema.type <- "random"
  
  return(this.df)
})

###############################################################################
# CHECK: determine the number of MISSED EMAs we have per person
###############################################################################
max.ema.count.missed <- lapply(list.df.raw.random.STATUS.cc2, function(x){
  df <- data.frame(user.id = unique(x$user.id),
                   M = nrow(x))
  df$user.id <- as.character(df$user.id)
  return(df)
})

max.ema.count.missed <- do.call(rbind, max.ema.count.missed)

###############################################################################
# Grab raw data responses for each participant i and discard extraneous info:
# Items rated on a 5-point Likert scale - Strongly Disagree to Strongly Agree
###############################################################################
find.this.string <- "{Strongly Disagree},{Disagree},{Neutral},{Agree},{Strongly Agree}"

# collect.idx are question ids with responses find.this.string
collect.idx <- items.cc2 %>% 
  filter(question.options==find.this.string) %>%
  select(question.id)

# Change collect.idx from data frame to numeric array
collect.idx <- c(as.matrix(collect.idx))

# List to collect item responses
list.resp.cc2 <- list()
N <- length(list.df.raw.random.DATA.cc2)

for(i in 1:N){
  df.raw <- list.df.raw.random.DATA.cc2[[i]]
  
  # Create outcome variable/target
  check1 <- grepl("_response_", colnames(df.raw))
  check2 <- (!(grepl("_response_option_", colnames(df.raw))))
  keep.these.cols <- check1 & check2
  drop.these.cols <- !keep.these.cols
  drop.these.cols <- colnames(df.raw)[drop.these.cols]
  # Calling the function CheckAnyResponse() creates with.any.response
  df.raw <- CheckAnyResponse(df = df.raw, drop.cols = drop.these.cols)
  
  # Proceed with remaining steps ----------------------------------------------
  # Bring time variables to the left of the data frame
  # prompt.ts and begin.ts are two time variables used to anchor analyses
  df.raw <- df.raw %>% 
    select(user.id, random.ema.id, ema.type, status,
           start_time, end_time,
           mCerebrum_timestamp, mCerebrum_offset, everything()) %>%
    rename(mCerebrum.ts = mCerebrum_timestamp) %>%
    rename(prompt.ts = start_time) %>%
    mutate(begin.ts = NA,
           end.ts = NA)
  
  all.col.names <- colnames(df.raw)
  check <- (("questions_0_response_0") %in% all.col.names)
  
  if(isTRUE(check)){
    # In raw data: questions_0_prompt_time = 0 all throughout
    df.raw <- df.raw %>% 
      mutate(begin.ts = questions_0_finish_time) %>%
      mutate(begin.ts = replace(begin.ts, 
                                (status=="ABANDONED_BY_TIMEOUT") & (questions_0_finish_time==0), 
                                NA)) %>%
      mutate(end.ts = end_time) %>%
      mutate(end.ts = replace(end.ts, is.na(begin.ts), NA))
    
    list.items <- list()
    # There are 64 items in a Random EMA for CC2 participants
    # The last item is a thank you message
    # Thus, idx ranges from 1 to 63
    for(idx in collect.idx){  
      these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
      these.col.names <- all.col.names[these.col.names]
      df.item <- df.raw %>% select(these.col.names) 
      
      # Note: Column names from CC1 to CC2 changed
      # For each of the EMA items, get:
      # question response: question_idx_response_0
      idx.resp <- match(x = paste("questions_",idx,"_response_0", sep=""), table = these.col.names)
      
      if(!is.na(idx.resp)){
        # Collect info into one data frame
        question.resp <- df.item[these.col.names[idx.resp]]
        question.resp <- as.data.frame(question.resp)
        question.resp <- CleanLikertScale(df = question.resp, col.name = colnames(question.resp))
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }else{
        question.resp <- rep(NA, times = nrow(df.item))
        question.resp <- as.data.frame(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }
    }
    
    df.resp <- do.call(cbind, list.items)
    df <- df.raw %>% select(user.id, random.ema.id, ema.type, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts,
                            with.any.response)
    df <- cbind(df, df.resp)
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }else{
    df.resp <- rep(NA, nrow(df.raw)*length(collect.idx))
    df.resp <- matrix(df.resp, ncol = length(collect.idx))
    df.resp <- as.data.frame(df.resp)
    colnames(df.resp) <- paste("item.",collect.idx, sep="")
    df <- df.raw %>% select(user.id, random.ema.id, ema.type, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts,
                            with.any.response)
    df <- cbind(df, df.resp) 
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }
}

df.resp.cc2 <- do.call(rbind, list.resp.cc2)
remove(list.resp.cc2, df.resp, df)

###############################################################################
# Prepare info on MISSED Random EMAs from STATUS files
# for merging with COMPLETED and ABANDONED_BY_TIMEOUT Random EMAs from
# DATA files
###############################################################################
list.df.tmp <- lapply(list.df.raw.random.STATUS.cc2, function(x, use.col.names=colnames(df.resp.cc2)){
  
  df.tmp <- matrix(NA, nrow(x), length(use.col.names))
  df.tmp <- as.data.frame(df.tmp)
  colnames(df.tmp) <- use.col.names
  df.tmp$user.id <- x$user.id
  df.tmp$random.ema.id <- x$random.ema.id
  df.tmp$ema.type <- x$ema.type
  df.tmp$status <- x$status
  df.tmp$prompt.ts <- x$prompt.ts
  # with.any.response=0 for all MISSED EMAs
  df.tmp$with.any.response <- rep(0, times=nrow(x))
  
  return(df.tmp)
})

df.tmp <- do.call(rbind, list.df.tmp)
remove(list.df.tmp)

###############################################################################
# Merge data in df.resp.cc2 and df.tmp
###############################################################################
df.resp.cc2 <- rbind(df.resp.cc2, df.tmp)

# Now convert timestamps from milliseconds to seconds
df.resp.cc2 <- df.resp.cc2 %>% 
  arrange(user.id, prompt.ts) %>%
  group_by(user.id) %>%
  mutate(prompt.ts = prompt.ts/1000,
         begin.ts = begin.ts/1000,
         end.ts = end.ts/1000,
         mCerebrum.ts = mCerebrum.ts/1000)

###############################################################################
# Write out df.resp.cc2 to a csv file
###############################################################################
write.out <- FALSE

if(isTRUE(write.out)){
  write.csv(df.resp.cc2, 
            file.path(path.breakfree.output_data, "resp.random.ema.cc2.csv"), 
            row.names=FALSE)
}

###############################################################################
# Grab raw data responses for each participant i and discard extraneous info:
# Items related to smoking
###############################################################################
# collect.idx are question ids
collect.idx <- c(39, 40, 41, 42, 43)

# Change collect.idx from data frame to numeric array
collect.idx <- c(as.matrix(collect.idx))

# List to collect item responses
list.resp.cc2 <- list()
N <- length(list.df.raw.random.DATA.cc2)


for(i in 1:N){
  df.raw <- list.df.raw.random.DATA.cc2[[i]]
  
  # Begin ---------------------------------------------------------------------
  # Bring time variables to the left of the data frame
  # prompt.ts and begin.ts are two time variables used to anchor analyses
  df.raw <- df.raw %>% 
    select(user.id, random.ema.id, ema.type, status,
           start_time, end_time,
           mCerebrum_timestamp, mCerebrum_offset, everything()) %>%
    rename(mCerebrum.ts = mCerebrum_timestamp) %>%
    rename(prompt.ts = start_time) %>%
    mutate(begin.ts = NA,
           end.ts = NA)
  
  all.col.names <- colnames(df.raw)
  check <- (("questions_0_response_0") %in% all.col.names)
  
  if(isTRUE(check)){
    # In raw data: questions_0_prompt_time = 0 all throughout
    df.raw <- df.raw %>% 
      mutate(begin.ts = questions_0_finish_time) %>%
      mutate(begin.ts = replace(begin.ts, 
                                (status=="ABANDONED_BY_TIMEOUT") & (questions_0_finish_time==0), 
                                NA)) %>%
      mutate(end.ts = end_time) %>%
      mutate(end.ts = replace(end.ts, is.na(begin.ts), NA))
    
    list.items <- list()
    for(idx in collect.idx){  
      these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
      these.col.names <- all.col.names[these.col.names]
      df.item <- df.raw %>% select(these.col.names) 
      
      # Note: Column names from CC1 to CC2 changed
      # For each of the EMA items, get:
      # question response: question_idx_response_0
      idx.resp <- match(x = paste("questions_",idx,"_response_0", sep=""), table = these.col.names)
      
      if(!is.na(idx.resp)){
        # Collect info into one data frame
        question.resp <- df.item[these.col.names[idx.resp]]
        question.resp <- as.data.frame(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }else{
        question.resp <- rep(NA, times = nrow(df.item))
        question.resp <- as.data.frame(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }
    }
    
    df.resp <- do.call(cbind, list.items)
    df <- df.raw %>% select(user.id, random.ema.id, ema.type, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts)
    df <- cbind(df, df.resp)
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }else{
    df.resp <- rep(NA, nrow(df.raw)*length(collect.idx))
    df.resp <- matrix(df.resp, ncol = length(collect.idx))
    df.resp <- as.data.frame(df.resp)
    colnames(df.resp) <- paste("item.",collect.idx, sep="")
    df <- df.raw %>% select(user.id, random.ema.id, ema.type, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts)
    df <- cbind(df, df.resp) 
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }
}

df.smoking.items.cc2 <- do.call(rbind, list.resp.cc2)
remove(list.resp.cc2, df.resp, df)

###############################################################################
# Prepare info on MISSED Random EMAs from STATUS files
# for merging with COMPLETED and ABANDONED_BY_TIMEOUT Random EMAs from
# DATA files
###############################################################################
list.df.tmp <- lapply(list.df.raw.random.STATUS.cc2, function(x, use.col.names=colnames(df.smoking.items.cc2)){
  
  df.tmp <- matrix(NA, nrow(x), length(use.col.names))
  df.tmp <- as.data.frame(df.tmp)
  colnames(df.tmp) <- use.col.names
  df.tmp$user.id <- x$user.id
  df.tmp$random.ema.id <- x$random.ema.id
  df.tmp$ema.type <- x$ema.type
  df.tmp$status <- x$status
  df.tmp$prompt.ts <- x$prompt.ts
  
  return(df.tmp)
})

df.tmp <- do.call(rbind, list.df.tmp)
remove(list.df.tmp)

###############################################################################
# Merge data in df.smoking.items.cc2 and df.tmp
###############################################################################
df.smoking.items.cc2 <- rbind(df.smoking.items.cc2, df.tmp)

# Convert missing values to proper class types
df.smoking.items.cc2 <- df.smoking.items.cc2 %>%
  mutate(item.39 = as.character(item.39),
         item.40 = as.numeric(item.40),
         item.41 = as.character(item.41),
         item.42 = as.character(item.42),
         item.43 = as.character(item.43)) %>%
  mutate(item.39 = if_else(item.39=="", NA_character_, item.39),
         item.41 = if_else(item.41=="", NA_character_, item.41),
         item.42 = if_else(item.42=="", NA_character_, item.42),
         item.43 = if_else(item.43=="", NA_character_, item.43))

# Now convert timestamps from milliseconds to seconds
df.smoking.items.cc2 <- df.smoking.items.cc2 %>% 
  arrange(user.id, prompt.ts) %>%
  group_by(user.id) %>%
  mutate(prompt.ts = prompt.ts/1000,
         begin.ts = begin.ts/1000,
         end.ts = end.ts/1000,
         mCerebrum.ts = mCerebrum.ts/1000)

###############################################################################
# Write out df.smoking.items.cc2 to a csv file
###############################################################################
write.out <- FALSE

if(isTRUE(write.out)){
  write.csv(df.smoking.items.cc2, 
            file.path(path.breakfree.output_data, "df.smoking.items.cc2.csv"), 
            row.names=FALSE)
}


