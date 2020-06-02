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
load(file.path(path.breakfree.staged_data, "raw.smokingEMA.cc1.RData"))

###############################################################################
# Create a question bank file containing info for each EMA item
###############################################################################
df.raw <- list.df.raw.smoking.DATA.cc1[[3]]  # Pick one individual

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
write.out <- TRUE

if(isTRUE(write.out)){
  write.csv(items.cc1, 
            file.path(path.breakfree.output_data, "smokingEMA_items_cc1.csv"), 
            row.names=FALSE, na="")
}

###############################################################################
# Data preprocessing steps
###############################################################################
N <- length(list.df.raw.smoking.DATA.cc1)
list.df.reference <- list()

for(i in 1:N){
  df.raw <- list.df.raw.smoking.DATA.cc1[[i]]
  
  # Create an id-variable for each ema in the raw data having
  # for merging responses within a participant
  df.raw$merge.id <- 1:nrow(df.raw)
  
  # Create outcome variable/target
  check1 <- grepl("_response_", colnames(df.raw))
  check2 <- (!(grepl("_response_option_", colnames(df.raw))))
  keep.these.cols <- check1 & check2
  keep.these.cols <- colnames(df.raw)[keep.these.cols]
  # Calling the function CheckAnyResponse() creates with.any.response
  df.raw <- CheckAnyResponse(df = df.raw, keep.cols = keep.these.cols)
  
  # Proceed with remaining steps 
  # Bring time variables to the left of the data frame
  # prompt.unixts and begin.unixts are two time variables used to anchor analyses
  df.raw <- df.raw %>% 
    select(user.id, status,
           start_timestamp, end_timestamp,
           everything()) %>%
    rename(prompt.unixts = start_timestamp) %>%
    mutate(status = as.character(status)) %>%
    mutate(begin.unixts = NA,
           end.unixts = NA)
  
  all.col.names <- colnames(df.raw)
  check <- (("question_answers_0_response_0") %in% all.col.names)
  
  if(isTRUE(check)){
    # Use question_answers_0_finish_time instead of 
    # question_answers_0_prompt_time for CC1 data
    # since in CC2 data, prompt times are zero throughout
    df.raw <- df.raw %>% 
      mutate(begin.unixts = question_answers_0_finish_time) %>%
      mutate(begin.unixts = replace(begin.unixts, 
                                    (status == "ABANDONED_BY_TIMEOUT") & (question_answers_0_finish_time==-1),
                                    NA)) %>%
      mutate(end.unixts = end_timestamp) %>%
      mutate(end.unixts = replace(end.unixts, is.na(begin.unixts), NA))
  }
  
  # Rearrange columns
  df.raw <- df.raw %>%
    select(user.id, merge.id, status, 
           prompt.unixts, begin.unixts, end.unixts, everything())
  
  df.ref <- df.raw %>%
    select(user.id, merge.id, status, 
           prompt.unixts, begin.unixts, end.unixts,
           with.any.response)
  
  # Save changes
  list.df.raw.smoking.DATA.cc1[[i]] <- df.raw
  list.df.reference <- append(list.df.reference, list(df.ref))
}

df.reference <- bind_rows(list.df.reference)

################################################################################
# Create a list to collect various types of responses
###############################################################################
list.collect.all <- list()

###############################################################################
# Grab raw data responses for each participant i and discard extraneous info:
# multiple_choice items
###############################################################################
find.this.string <- "multiple_choice"

# collect.idx are question ids with response type find.this.string
collect.idx <- items.cc1 %>% 
  filter(question.type==find.this.string) %>%
  select(question.id)

# Change collect.idx from data frame to numeric array
collect.idx <- c(as.matrix(collect.idx))

# Grab items with responses find.this.string
source(file.path(path.breakfree.code, "data-stream-smoking-ema/grab-single-response-items-cc1.R"))
df.resp.cc1 <- df.resp.cc1 %>% mutate_at(vars(grep(pattern = "item.", x = .)), as.character)
list.collect.all <- append(list.collect.all, list(df.resp.cc1))

# Remove these variables from environment so that they can be reused
remove(list.resp.cc1, df.resp, df, df.resp.cc1)

###############################################################################
# Grab raw data responses for each participant i and discard extraneous info:
# text_numeric items
###############################################################################
find.this.string <- "text_numeric"

# collect.idx are question ids with response type find.this.string
collect.idx <- items.cc1 %>% 
  filter(question.type==find.this.string) %>%
  select(question.id)

# Change collect.idx from data frame to numeric array
collect.idx <- c(as.matrix(collect.idx))

# Grab items with responses find.this.string
source(file.path(path.breakfree.code, "data-stream-smoking-ema/grab-single-response-items-cc1.R"))
df.resp.cc1 <- df.resp.cc1 %>% mutate_at(vars(grep(pattern = "item.", x = .)), as.character)
list.collect.all <- append(list.collect.all, list(df.resp.cc1))

# Remove these variables from environment so that they can be reused
remove(list.resp.cc1, df.resp, df, df.resp.cc1)

###############################################################################
# Grab raw data responses for each participant i and discard extraneous info:
# multiple_select items
###############################################################################
find.this.string <- "multiple_select"

# collect.idx are question ids with response type find.this.string
collect.idx <- items.cc1 %>% 
  filter(question.type==find.this.string) %>%
  select(question.id)

# Change collect.idx from data frame to numeric array
collect.idx <- c(as.matrix(collect.idx))

# Grab items with responses find.this.string
source(file.path(path.breakfree.code, "data-stream-smoking-ema/grab-multiple-response-items-cc1.R"))
df.resp.cc1 <- df.resp.cc1 %>% mutate_at(vars(grep(pattern = "item.", x = .)), as.character)

list.collect.all <- append(list.collect.all, list(df.resp.cc1))

# Remove these variables from environment so that they can be reused
remove(list.resp.cc1, df.resp, df, df.resp.cc1)

###############################################################################
# Merge different types of information into one data frame per participant
###############################################################################
df.collect.all <- list.collect.all %>% reduce(left_join, by = c("user.id", "merge.id"))
df.collect.all <- left_join(x = df.collect.all, y = df.reference, by = c("user.id", "merge.id"))
df.collect.all <- df.collect.all %>% select(-merge.id)

# Remove these variables from environment 
remove(list.collect.all)

###############################################################################
# Convert timestamps from milliseconds to seconds
# and add human-readable time
###############################################################################
df.collect.all <- df.collect.all %>% 
  arrange(user.id, prompt.unixts) %>%
  group_by(user.id) %>%
  mutate(prompt.unixts = prompt.unixts/1000,
         begin.unixts = begin.unixts/1000,
         end.unixts = end.unixts/1000)

# Add human-readable timestamps
df.collect.all <- df.collect.all %>%
  mutate(prompt.hrts = as.POSIXct(prompt.unixts, tz = "CST6CDT", origin="1970-01-01"),
         begin.hrts = as.POSIXct(begin.unixts, tz = "CST6CDT", origin="1970-01-01"),
         end.hrts = as.POSIXct(end.unixts, tz = "CST6CDT", origin="1970-01-01")) %>%
  mutate(prompt.hrts = strftime(prompt.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"),
         begin.hrts = strftime(begin.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"),
         end.hrts = strftime(end.hrts, format="%Y-%m-%d %H:%M:%S %z", tz = "CST6CDT"))%>%
  mutate(prompt.hrts = as.character(prompt.hrts),
         begin.hrts = as.character(begin.hrts),
         end.hrts = as.character(end.hrts))

df.collect.all$timezone.hrts <- "CST6CDT"

###############################################################################
# Other data preparation steps
###############################################################################

# Add type and status
df.collect.all <- df.collect.all %>% 
  mutate(ema.type = "smoking") %>%
  group_by(user.id) %>%
  mutate(ones=1) %>%
  mutate(smoking.ema.id = cumsum(ones)) %>%
  select(-ones)

# Rearrange columns
tmp.idx <- grep(pattern = "item.", x = colnames(df.collect.all))
tmp.item.numbers <- substring(colnames(df.collect.all)[tmp.idx], first = 6)
tmp.item.numbers <- as.numeric(tmp.item.numbers)
tmp.item.numbers <- tmp.item.numbers[order(tmp.item.numbers)]
tmp.item.names <- paste("item.",tmp.item.numbers,sep="")

df.collect.all <- df.collect.all %>%
  select(user.id, smoking.ema.id, ema.type, status,
         prompt.hrts, begin.hrts, end.hrts, timezone.hrts,
         prompt.unixts, begin.unixts, end.unixts,
         with.any.response,
         tmp.item.names,
         everything())

###############################################################################
# Write out df.collect.all to a csv file
###############################################################################
write.out <- TRUE

if(isTRUE(write.out)){
  write.csv(df.collect.all, 
            file.path(path.breakfree.output_data, "smokingEMA_responses_cc1.csv"), 
            row.names=FALSE, na="")
}

