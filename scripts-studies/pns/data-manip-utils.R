###############################################################################
# ABOUT:
# * Set of functions specifically for working with PNS data
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

CreateEMATimeVars <- function(df.raw){
  # About: Common data pre-processing tasks for PNS EMA data
  # Args: 
  #   df.raw : raw data from one type of EMA only
  # Output:
  #   dataset with EMA time variables
  
  # --------------------------------------------------------------------------- 
  # Rename variables in the raw data that will be relevant to the tasks
  # within this function
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
           cancelled.hrts = AssessmentCancelled,
           responded = Responded,
           completed = Completed)
  
  # --------------------------------------------------------------------------- 
  # Format time variables
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    mutate(delivered.hrts = as.POSIXct(strptime(delivered.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")),
           begin.hrts = as.POSIXct(strptime(begin.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")),
           completed.hrts = as.POSIXct(strptime(completed.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")),
           notcompleted.hrts = as.POSIXct(strptime(notcompleted.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")),
           cancelled.hrts = as.POSIXct(strptime(cancelled.hrts, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"))) %>%
    mutate(delivered.unixts = as.numeric(delivered.hrts),
           begin.unixts = as.numeric(begin.hrts))
  
  # ---------------------------------------------------------------------------
  # Decision rule: create end.hrts and end.unixts
  # ---------------------------------------------------------------------------
  df.out <- df.out %>% 
    mutate(end.hrts = completed.hrts) %>%
    mutate(end.hrts = if_else(is.na(completed.hrts), notcompleted.hrts, end.hrts)) %>%
    mutate(end.hrts = if_else(is.na(end.hrts) & !is.na(cancelled.hrts), cancelled.hrts, end.hrts)) %>%
    mutate(end.unixts = as.numeric(end.hrts))
  
  # ---------------------------------------------------------------------------
  # Decision rule: exclude EMAs that have some indication of unsuccessful 
  # delivery. For now, simply create an indicator function is.delivered and
  # perform the exclusion step outside of this function
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    mutate(responded = if_else(responded=="","Missing",responded), completed = if_else(completed=="","Missing",completed)) %>%
    mutate(is.delivered = if_else((responded=="True" & completed=="True")|(responded=="True" & completed=="False")|(responded=="True" & completed=="Missing")|(responded=="Missing" & completed=="False"), 1, 0))
  
  # --------------------------------------------------------------------------- 
  # Reorder columns
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    select(id, record.id, assessment.type,
           delivered.hrts, begin.hrts, end.hrts,
           delivered.unixts, begin.unixts, end.unixts,
           record.status, responded, completed, is.delivered,
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


