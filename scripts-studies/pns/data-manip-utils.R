###############################################################################
# ABOUT:
# * Set of functions specifically for working with PNS data
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

SetPostQuitTimeFrame <- function(df.quit.dates, postquit.duration, addtime, remove.missing=TRUE){
  # About: Calculate start of clock and end of clock in unix time
  #   from list of participant quit dates
  # Args:
  #   df.quit.dates: list of participant quit dates in YYYY-MM-DD format
  #   postquit.duration: the duration of the study in days
  #   addtime: time difference in hours of unix timestamps created 
  #     on local machine and unix timestamps in raw data
  #   remove.missing: if TRUE, individuals with missing quit dates will be removed;
  #     if FALSE, individuals with missing quit dates will not be removed;
  #     by default, we set remove.missing to TRUE
  # Output:
  #   data frame with start of clock and end of clock in unix time
  
  # Check that inputs are what is expected ------------------------------------
  # (1) Check: column names are correct
  check <- sum(!hasName(df.quit.dates, c("id","callnumr","quit.date")))
  assert_that(check==0, msg = "Column names must be: id, callnumr, quit.date")
  
  # (2) Check: whether there are individuals with missing quit dates
  tmp.date.df <- df.quit.dates
  tmp.date.df[["quit.date"]] <- as.character(tmp.date.df[["quit.date"]])
  tmp.date.df[["quit.date"]] <- if_else(tmp.date.df[["quit.date"]]=="", 
                                        NA_character_, 
                                        tmp.date.df[["quit.date"]])
  any.missing <- sum(is.na(tmp.date.df[["quit.date"]]))
  if(any.missing > 0){
    tmp.date.df <- tmp.date.df %>% filter(!is.na(quit.date))
  }
  
  # (3) Check: quit date is in the correct format (after removing individuals
  #     with missing quit dates in tmp.date.df)
  tmp.date.df <- strptime(tmp.date.df[["quit.date"]], format = "%Y-%m-%d")
  # Incorrectly formatted date will yield check=NA
  check <- sum(is.na(tmp.date.df))
  assert_that(check==0, msg = "quit.date MUST BE IN YYYY-MM-DD FORMAT")
  remove(tmp.date.df)
  
  # Begin ---------------------------------------------------------------------
  df.quit.dates[["quit.date"]] <- as.character(df.quit.dates[["quit.date"]])
  df.quit.dates[["quit.date"]] <- if_else(df.quit.dates[["quit.date"]]=="", 
                                          NA_character_, 
                                          df.quit.dates[["quit.date"]])
  any.missing <- sum(is.na(df.quit.dates[["quit.date"]]))
  if(any.missing > 0 & isTRUE(remove.missing)){
    df.quit.dates <- df.quit.dates %>% filter(!is.na(quit.date))
  }
  
  # Calculate timestamps corresponding to start and end of clock
  df.quit.dates[["quit.date"]] <- strptime(df.quit.dates[["quit.date"]], format = "%Y-%m-%d")
  df.quit.dates[["quit.date"]] <- as.POSIXct(df.quit.dates[["quit.date"]])
  df.quit.dates$start.clock.unixts <- NA
  df.quit.dates$end.clock.unixts <- NA
  
  # 4AM on Quit Day 
  df.quit.dates[["start.clock.unixts"]] <- as.numeric(df.quit.dates[["quit.date"]]) + 4*60*60 
  # 12AM on postquit.duration days after Quit Day
  total.duration <- postquit.duration*24*60*60  - 4*60*60
  df.quit.dates[["end.clock.unixts"]] <- df.quit.dates[["start.clock.unixts"]] + total.duration
  
  # Adjust for time zone differences
  df.quit.dates[["start.clock.unixts"]] <- df.quit.dates[["start.clock.unixts"]] + addtime*60*60 
  df.quit.dates[["end.clock.unixts"]] <- df.quit.dates[["end.clock.unixts"]] + addtime*60*60
  
  # Create interpretable timestamps
  df.quit.dates[["start.clock.hrts"]] <- as.POSIXct(df.quit.dates[["start.clock.unixts"]], origin="1970-01-01")
  df.quit.dates[["end.clock.hrts"]] <- as.POSIXct(df.quit.dates[["end.clock.unixts"]], origin="1970-01-01")
  
  # ---------------------------------------------------------------------------
  # Clean up output
  # ---------------------------------------------------------------------------
  df.quit.dates[["quit.date"]] <- strftime(df.quit.dates[["quit.date"]], format = "%Y-%m-%d")
  df.quit.dates[["quit.date"]] <- as.character(df.quit.dates[["quit.date"]])
  row.names(df.quit.dates) <- 1:nrow(df.quit.dates)
  df.time.frame <- df.quit.dates %>% 
    select(id, callnumr, quit.date, 
           start.clock.hrts, end.clock.hrts, 
           start.clock.unixts, end.clock.unixts)
  
  return(df.time.frame)
}


SetUpPostQuit <- function(df.raw){
  # About: Common data pre-processing tasks for PNS post quit raw data
  # Args: 
  #   df.raw : one of post quit random, post quit urge,
  #     post quit about to slip, post quit about to slip part 2,
  #     post quit already slipped raw data
  # Output:
  #   dataset with rows that meet inclusion-exclusion criteria
  
  # --------------------------------------------------------------------------- 
  # Rename variables in the raw data that will be relevant to the tasks in the
  # SetUpPostQuit function
  # ---------------------------------------------------------------------------
  df.out <- df.raw %>%
    rename(id = Part_ID, 
           record.id = Record_ID,
           record.status = Record_Status,
           assessment.type = Asse_Name, 
           delivered.hrts = Initiated,
           begin.hrts = AssessmentBegin,
           completed.hrts = AssessmentCompleted,
           notcompleted.hrts = AssessmentNOTCompleted,
           responded = Responded,
           completed = Completed) %>%
    mutate(id = as.character(id),
           record.id = as.character(record.id),
           record.status = as.character(record.status),
           assessment.type = as.character(assessment.type),
           delivered.hrts = as.character(delivered.hrts),
           begin.hrts = as.character(begin.hrts),
           completed.hrts = as.character(completed.hrts),
           notcompleted.hrts = as.character(notcompleted.hrts),
           responded = as.character(responded),
           completed = as.character(completed))
  
  # --------------------------------------------------------------------------- 
  # Format time variables
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    mutate(delivered.hrts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p")),
           begin.hrts = as.POSIXct(strptime(begin.hrts, format = "%m/%d/%Y %I:%M:%S %p")),
           completed.hrts = as.POSIXct(strptime(completed.hrts, format = "%m/%d/%Y %I:%M:%S %p")),
           notcompleted.hrts = as.POSIXct(strptime(notcompleted.hrts, format = "%m/%d/%Y %I:%M:%S %p"))) %>%
    mutate(delivered.unixts = as.numeric(delivered.hrts),
           begin.unixts = as.numeric(begin.hrts))
  
  # ---------------------------------------------------------------------------
  # Decision rule: exclude EMAs that are "not valid"
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    mutate(responded = if_else(responded=="","Missing",responded),
           completed = if_else(completed=="","Missing",completed)) %>%
    filter((responded=="True" & completed=="True")|
             (responded=="True" & completed=="False")|
             (responded=="True" & completed=="Missing")|
             (responded=="Missing" & completed=="False"))
  
  # ---------------------------------------------------------------------------
  # Decision rule: create end.hrts and end.unixts
  # ---------------------------------------------------------------------------
  df.out <- df.out %>% 
    mutate(end.hrts = completed.hrts) %>%
    mutate(end.hrts = if_else(is.na(completed.hrts), notcompleted.hrts, end.hrts)) %>%
    mutate(end.unixts = as.numeric(end.hrts))
  
  # --------------------------------------------------------------------------- 
  # Reorder columns
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    select(id, record.id, assessment.type,
           delivered.hrts, begin.hrts, end.hrts,
           delivered.unixts, begin.unixts, end.unixts,
           record.status, responded, completed,
           everything())
  
  return(df.out)
}


SetUpPreQuit <- function(df.raw){
  # About: Common data pre-processing tasks for PNS post quit raw data
  # Args: 
  #   df.raw : one of pre quit random, pre quit urge,
  #     pre quit about to slip, pre quit about to slip part 2 raw data
  # Output:
  #   dataset with rows that meet inclusion-exclusion criteria
  
  # --------------------------------------------------------------------------- 
  # Rename variables in the raw data that will be relevant to the tasks in the
  # SetUpPreQuit function
  # ---------------------------------------------------------------------------
  df.out <- df.raw %>%
    rename(id = Part_ID, 
           record.id = Record_ID,
           record.status = Record_Status,
           assessment.type = Asse_Name, 
           delivered.hrts = Initiated,
           begin.hrts = AssessmentBegin,
           completed.hrts = AssessmentCompleted,
           notcompleted.hrts = AssessmentNOTCompleted,
           responded = Responded,
           completed = Completed) %>%
    mutate(id = as.character(id),
           record.id = as.character(record.id),
           record.status = as.character(record.status),
           assessment.type = as.character(assessment.type),
           delivered.hrts = as.character(delivered.hrts),
           begin.hrts = as.character(begin.hrts),
           completed.hrts = as.character(completed.hrts),
           notcompleted.hrts = as.character(notcompleted.hrts),
           responded = as.character(responded),
           completed = as.character(completed))
  
  # --------------------------------------------------------------------------- 
  # Format time variables
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    mutate(delivered.hrts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p")),
           begin.hrts = as.POSIXct(strptime(begin.hrts, format = "%m/%d/%Y %I:%M:%S %p")),
           completed.hrts = as.POSIXct(strptime(completed.hrts, format = "%m/%d/%Y %I:%M:%S %p")),
           notcompleted.hrts = as.POSIXct(strptime(notcompleted.hrts, format = "%m/%d/%Y %I:%M:%S %p"))) %>%
    mutate(delivered.unixts = as.numeric(delivered.hrts),
           begin.unixts = as.numeric(begin.hrts))
  
  # ---------------------------------------------------------------------------
  # Decision rule: exclude EMAs that are "not valid"
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    mutate(responded = if_else(responded=="","Missing",responded),
           completed = if_else(completed=="","Missing",completed)) %>%
    filter((responded=="True" & completed=="True")|
             (responded=="True" & completed=="False")|
             (responded=="True" & completed=="Missing")|
             (responded=="Missing" & completed=="False"))
  
  # ---------------------------------------------------------------------------
  # Decision rule: create end.hrts and end.unixts
  # ---------------------------------------------------------------------------
  df.out <- df.out %>% 
    mutate(end.hrts = completed.hrts) %>%
    mutate(end.hrts = if_else(is.na(completed.hrts), notcompleted.hrts, end.hrts)) %>%
    mutate(end.unixts = as.numeric(end.hrts))
  
  # --------------------------------------------------------------------------- 
  # Reorder columns
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    select(id, record.id, assessment.type,
           delivered.hrts, begin.hrts, end.hrts,
           delivered.unixts, begin.unixts, end.unixts,
           record.status, responded, completed,
           everything())
  
  return(df.out)
}

SearchRecordID <- function(timestamp, df.covariate, past = TRUE){
  # About: Gets record.id of most proximal EMA in the prior to (if
  #        past==TRUE) timestamp or right after (if past==FALSE)
  #        timestamp
  # Args: 
  #   timestamp: a timestamp
  #   df.covariate: a data frame whose rows are records of
  #     responses to Random EMA items
  #   past: TRUE or FALSE to indicate whether a past or future
  #     record is desired
  # Output:
  #   record.id of most proximal EMA in the prior to (if
  #   past==TRUE) timestamp or right after (if past==FALSE)
  #   timestamp
  
  assert_that(is.numeric(timestamp), msg = "timestamp must be in numeric format")
  
  if(isTRUE(past)){
    # Get record.id from most proximal random EMA prior to timestamp
    collect.idx <- which(df.covariate$time.unixts <= timestamp)
    if(length(collect.idx) == 0){
      this.record.id <- NA_character_
    }else{
      this.record.id <- as.character(df.covariate[max(collect.idx), ]$record.id)
    }
  }else{ 
    # Get record.id from most proximal random EMA after timestamp
    collect.idx <- which(df.covariate$time.unixts >= timestamp)
    if(length(collect.idx) == 0){
      this.record.id <- NA_character_
    }else{
      this.record.id <- as.character(df.covariate[min(collect.idx), ]$record.id)
    }
  }
  
  return(this.record.id)
}


