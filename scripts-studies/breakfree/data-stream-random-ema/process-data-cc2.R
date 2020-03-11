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
write.csv(items.cc2, 
          file.path(path.breakfree.output_data, "random.ema.items.cc2.csv"), 
          row.names=FALSE)

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
  
  # Proceed with remaining steps
  df.raw$ema.count <- 1:nrow(df.raw)
  # Bring time variables to the left of the data frame
  # prompt.ts and begin.ts are two time variables used to anchor analyses
  df.raw <- df.raw %>% 
    select(user.id, ema.count, status,
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
    df <- df.raw %>% select(user.id, ema.count, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts,
                            with.any.response)
    df <- cbind(df, df.resp)
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }else{
    df.resp <- rep(NA, nrow(df.raw)*length(collect.idx))
    df.resp <- matrix(df.resp, ncol = length(collect.idx))
    df.resp <- as.data.frame(df.resp)
    colnames(df.resp) <- paste("item.",collect.idx, sep="")
    df <- df.raw %>% select(user.id, ema.count, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts,
                            with.any.response)
    df <- cbind(df, df.resp) 
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }
}

df.resp.cc2 <- do.call(rbind, list.resp.cc2)
remove(df.resp, df)

###############################################################################
# CHECK: determine the number of EMAs we have thus far per person
###############################################################################
max.ema.count <- lapply(list.resp.cc2, function(x){
  df <- data.frame(user.id = unique(x$user.id),
                   M = max(x$ema.count))
  df$user.id <- as.character(df$user.id)
  return(df)
})

max.ema.count <- do.call(rbind, max.ema.count)

###############################################################################
# Grab info on MISSED Random EMAs from STATUS files
###############################################################################
# Obtain columns in each person's STATUS file corresponding to MISSED EMAs
list.df.tmp <- lapply(list.df.raw.random.STATUS.cc2, function(x){
  
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

# Remove NULL elements of the list list.df.tmp
list.df.tmp <- discard(list.df.tmp, is.null)

# Prepare to merge data frames in list.df.tmp with df.resp.cc2
list.df.tmp <- lapply(list.df.tmp, function(x, use.col.names=colnames(df.resp.cc2)){
  
  df.tmp <- matrix(NA, nrow(x), length(use.col.names))
  df.tmp <- as.data.frame(df.tmp)
  colnames(df.tmp) <- colnames(df.resp.cc2)
  df.tmp$user.id <- x$user.id
  df.tmp$prompt.ts <- x$prompt.ts
  df.tmp$status <- x$status
  # with.any.response=0 for all MISSED EMAs
  df.tmp$with.any.response <- rep(0, times=nrow(x))
  # Note: nrow(x) is always positive since NULL elements of 
  # list.df.tmp have already been discarded
  df.tmp$ema.count <- seq(1,nrow(x))  
  
  return(df.tmp)
})

df.tmp <- do.call(rbind, list.df.tmp)

###############################################################################
# CHECK: determine the number of EMAs we have thus far per person
###############################################################################
max.ema.count.missed <- lapply(list.df.tmp, function(x){
  df <- data.frame(user.id = unique(x$user.id),
                   M = max(x$ema.count))
  df$user.id <- as.character(df$user.id)
  return(df)
})

max.ema.count.missed <- do.call(rbind, max.ema.count.missed)

###############################################################################
# Merge data in df.resp.cc2 and df.tmp
###############################################################################
df.resp.cc2 <- df.resp.cc2 %>% rename(ema.id = ema.count) %>% mutate(ema.id = NA)
df.tmp <- df.tmp %>% rename(ema.id = ema.count) %>% mutate(ema.id = NA)
df.resp.cc2 <- rbind(df.resp.cc2, df.tmp)

# Now construct the ema.id variable and convert timestamps from 
# milliseconds to seconds
df.resp.cc2 <- df.resp.cc2 %>% 
  arrange(user.id, prompt.ts) %>%
  group_by(user.id) %>% 
  mutate(ema.id = seq(1,n())) %>%
  mutate(prompt.ts = prompt.ts/1000,
         begin.ts = begin.ts/1000,
         end.ts = end.ts/1000,
         mCerebrum.ts = mCerebrum.ts/1000)

###############################################################################
# Write out df.resp.cc2 to a csv file
###############################################################################
write.csv(df.resp.cc2, 
          file.path(path.breakfree.output_data, "resp.random.ema.cc2.csv"), 
          row.names=FALSE)

