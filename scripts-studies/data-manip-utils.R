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
  #   df.raw, a data frame containing raw data from one type of EMA only
  # Output:
  #   df.raw, but with additional columns for time vriables
  
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


