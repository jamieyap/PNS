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
load(file.path(path.breakfree.staged_data, "raw.randomEMA.cc1.RData"))

###############################################################################
# Create a question bank file containing info for each EMA item
###############################################################################
df.raw <- list.df.raw.random.DATA.cc1[[3]]  # Pick one individual

list.items <- list()
all.col.names <- colnames(df.raw)
# There are 54 items in a Random EMA for CC1 participants
# The last item is a thank you message
# Thus, idx ranges from 0 to 52 (numbering begins at 0, not 1)
for(idx in 0:52){  
  these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
  these.col.names <- all.col.names[these.col.names]
  df.item <- df.raw %>% select(these.col.names)  
  df.item <- df.item[1,] # Just grab the first row
  
  # Note: Column names from CC1 to CC2 changed
  # For each of the EMA items, get:
  # question number: question_answers_idx_question_id
  # question text: question_answers_idx_question_text
  # question type: question_answers_idx_question_type
  # options for possible responses: question_answers_idx_response_option_nn
  idx.id <- match(x = paste("question_answers_",idx,"_question_id", sep=""), table = these.col.names)
  idx.text <- match(x = paste("question_answers_",idx,"_question_text", sep=""), table = these.col.names)
  idx.type <- match(x = paste("question_answers_",idx,"_question_type", sep=""), table = these.col.names)
  idx.options <- grep(pattern = paste("question_answers_",idx,"_response_option_", sep=""), x = these.col.names)
  
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

items.cc1 <- do.call(rbind, list.items)

# Write out items.cc1 to a csv file
write.csv(items.cc1, 
          file.path(path.breakfree.output_data, "random.ema.items.cc1.csv"), 
          row.names=FALSE)

###############################################################################
# Grab raw data responses for each participant i and discard extraneous info:
# Items rated on a 5-point Likert scale - Strongly Disagree to Strongly Agree
###############################################################################
find.this.string <- "{Strongly Disagree},{Disagree},{Neutral},{Agree},{Strongly Agree}"

# collect.idx are question ids with responses find.this.string
collect.idx <- items.cc1 %>% 
  filter(question.options==find.this.string) %>%
  select(question.id)

# Change collect.idx from data frame to numeric array
collect.idx <- c(as.matrix(collect.idx))

# List to collect item responses
list.resp.cc1 <- list()
N <- length(list.df.raw.random.DATA.cc1)

for(i in 1:N){
  df.raw <- list.df.raw.random.DATA.cc1[[i]]
  
  # Create outcome variable/target
  keep.these.cols <- grepl("_response_", colnames(df.raw)) & !(grepl("_response_option_", colnames(df.raw)))
  drop.these.cols <- !keep.these.cols
  drop.these.cols <- colnames(df.raw)[drop.these.cols]
  # Calling the function CheckAnyResponse() creates with.any.response
  df.raw <- CheckAnyResponse(df = df.raw, drop.cols = drop.these.cols)
  
  # Proceed with remaining steps 
  df.raw$ema.id <- 1:nrow(df.raw)
  # Bring time variables to the left of the data frame
  # prompt.ts and begin.ts are two time variables used to anchor analyses
  df.raw <- df.raw %>% 
    select(user.id, ema.id, status,
           start_timestamp, end_timestamp,
           mCerebrum_timestamp, mCerebrum_offset, everything()) %>%
    rename(mCerebrum.ts = mCerebrum_timestamp) %>%
    rename(prompt.ts = start_timestamp) %>%
    mutate(begin.ts = NA,
           end.ts = NA)
  
  all.col.names <- colnames(df.raw)
  check <- (("question_answers_0_response_0") %in% all.col.names)
  
  if(isTRUE(check)){
    # Use question_answers_0_finish_time instead of 
    # question_answers_0_prompt_time for CC1 data
    # since in CC2 data, prompt times are zero throughout
    df.raw <- df.raw %>% 
      mutate(begin.ts = question_answers_0_finish_time) %>%
      mutate(begin.ts = replace(begin.ts, 
                                (status == "ABANDONED_BY_TIMEOUT") & (question_answers_0_finish_time==-1),
                                NA)) %>%
      mutate(end.ts = end_timestamp) %>%
      mutate(end.ts = replace(end.ts, is.na(begin.ts), NA))
    
    list.items <- list()
    # There are 53 items in a Random EMA for CC1 participants
    # The last item is a thank you message
    # Thus, idx ranges from 1 to 52
    for(idx in collect.idx){  
      these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
      these.col.names <- all.col.names[these.col.names]
      df.item <- df.raw %>% select(these.col.names) 
      
      # Note: Column names from CC1 to CC2 changed
      # For each of the EMA items, get:
      # question response: question_answers_idx_response_0
      idx.resp <- match(x = paste("question_answers_",idx,"_response_0", sep=""), table = these.col.names)
      
      if(is.na(idx.resp)){
        # This column name occurs when there is no recorded response to this EMA item
        # at any point during the data collection period. This can occur when
        # all EMAs prompted are either MISSED or ABANDONED_BY_TIMEOUT
        idx.resp <- match(x = paste("question_answers_",idx,"_response", sep=""), table = these.col.names)
      }
      
      # Collect info into one data frame
      question.resp <- df.item[these.col.names[idx.resp]]
      question.resp <- as.data.frame(question.resp)
      question.resp <- CleanLikertScale(df = question.resp, col.name = colnames(question.resp))
      colnames(question.resp) <- paste("item.",idx, sep="")
      list.items <- append(list.items,list(question.resp))
    }
    
    df.resp <- do.call(cbind, list.items)
    df <- df.raw %>% select(user.id, ema.id, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts, 
                            with.any.response)
    df <- cbind(df, df.resp)
    list.resp.cc1 <- append(list.resp.cc1, list(df))
  }else{
    df.resp <- rep(NA, nrow(df.raw)*length(collect.idx))
    df.resp <- matrix(df.resp, ncol = length(collect.idx))
    df.resp <- as.data.frame(df.resp)
    colnames(df.resp) <- paste("item.",collect.idx, sep="")
    df <- df.raw %>% select(user.id, ema.id, status, 
                            prompt.ts, begin.ts, end.ts, mCerebrum.ts, 
                            with.any.response)
    df <- cbind(df, df.resp) 
    list.resp.cc1 <- append(list.resp.cc1, list(df))
  }
}

df.resp.cc1 <- do.call(rbind, list.resp.cc1)

# Convert timestamps from milliseconds to seconds
df.resp.cc1 <- df.resp.cc1 %>% mutate(prompt.ts = prompt.ts/1000,
                                      begin.ts = begin.ts/1000,
                                      end.ts = end.ts/1000,
                                      mCerebrum.ts = mCerebrum.ts/1000)

###############################################################################
# Write out df.resp.cc1 to a csv file
###############################################################################
write.csv(df.resp.cc1, 
          file.path(path.breakfree.output_data, "resp.random.ema.cc1.csv"), 
          row.names=FALSE)

