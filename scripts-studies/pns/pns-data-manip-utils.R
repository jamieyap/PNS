library(dplyr)
library(magrittr)
library(assertthat)

SetTimeFrame <- function(df.quit.dates, study.duration, addtime){
  # About: Calculate start of clock and end of clock in unix time
  #   from list of participant quit dates
  # Args:
  #   df.quit.dates: list of participant quit dates in MM/DD/YYYY format
  #   study.duration: the duration of the study in days
  #   addtime: time difference in hours of unix timestamps created 
  #     on local machine and unix timestamps in raw data
  # Output:
  #   data frame with start of clock and end of clock in unix time
  
  # Check that inputs are what is expected ------------------------------------
  # (1) Check: column names are correct
  check <- sum(!hasName(df.quit.dates, c("id","quit.date")))
  assert_that(check==0, msg = "Column names must be: id, quit.date")
  
  # (2) Check: quit date is in the correct format 
  tmp.date.df <- as.character(df.quit.dates[,"quit.date"])
  tmp.date.df <- strptime(tmp.date.df, format = "%m/%d/%Y")
  # Incorrectly formatted date will yield check=NA
  check <- sum(is.na(tmp.date.df))
  assert_that(check==0, msg = "quit.date MUST BE IN mm/dd/yyyy FORMAT")
  
  # Begin ---------------------------------------------------------------------
  df.quit.dates[,"quit.date"] <- as.character(df.quit.dates[,"quit.date"])
  
  # Calculate timestamps corresponding to start and end of clock
  df.quit.dates[,"quit.date"] <- as.POSIXct(strptime(df.quit.dates[,"quit.date"], format = "%m/%d/%Y"))
  df.quit.dates$start.clock <- NA
  df.quit.dates$end.clock <- NA
  
  # 4AM on Quit Day 
  df.quit.dates[,"start.clock"] <- as.numeric(df.quit.dates[,"quit.date"]) + 4*60*60 
  # 12AM on study.duration days after Quit Day
  total.duration <- study.duration*24*60*60  - 4*60*60
  df.quit.dates[,"end.clock"] <- df.quit.dates[,"start.clock"] + total.duration
  
  # Adjust for time zone differences
  df.quit.dates[,"start.clock"] <- df.quit.dates[,"start.clock"] + addtime*60*60 
  df.quit.dates[,"end.clock"] <- df.quit.dates[,"end.clock"] + addtime*60*60
  
  # ---------------------------------------------------------------------------
  # Clean up output
  # ---------------------------------------------------------------------------
  df.quit.dates[,"quit.date"] <- strftime(df.quit.dates[,"quit.date"], format = "%m/%d/%Y")
  df.quit.dates[,"quit.date"] <- as.character(df.quit.dates[,"quit.date"])
  row.names(df.quit.dates) <- 1:nrow(df.quit.dates)
  df.time.frame <- df.quit.dates
  
  return(df.time.frame)
}


SetUpPostQuit <- function(df.raw, df.time.frame){
  # About: Common data pre-processing tasks for PNS post quit raw data
  # Args: 
  #   df.raw : one of post quit random, post quit urge,
  #     post quit about to slip, post quit about to slip part 2,
  #     post quit already slipped raw data
  #   df.time.frame: output of SetTimeFrame()
  # Output:
  #   dataset with rows that meet inclusion-exclusion criteria
  
  # --------------------------------------------------------------------------- 
  # Rename variables in raw data and create new time variables
  # ---------------------------------------------------------------------------
  df.out <- df.raw %>%
    rename(id = Part_ID, 
           record.id = Record_ID,
           record.status = Record_Status,
           assessment.type = Asse_Name, 
           delivered.hrts = Initiated,
           assessment.hrts = AssessmentBegin) %>% 
    mutate(record.id = as.character(record.id),
           assessment.type = as.character(assessment.type), 
           delivered.hrts = as.character(delivered.hrts),
           assessment.hrts = as.character(assessment.hrts)) %>%
    mutate(delivered.unixts = as.POSIXct(strptime(delivered.hrts, 
                                                  format = "%m/%d/%Y %I:%M:%S %p", 
                                                  tz="EST5EDT")),
           assessment.unixts = as.POSIXct(strptime(assessment.hrts, 
                                                   format = "%m/%d/%Y %I:%M:%S %p", 
                                                   tz="EST5EDT"))) %>%
    mutate(delivered.unixts = as.numeric(delivered.unixts),
           assessment.unixts = as.numeric(assessment.unixts)) %>%
    mutate(delay = assessment.unixts - delivered.unixts)
  
  # --------------------------------------------------------------------------- 
  # Decision rule: exclude EMAs delivered before start of clock or
  # after end of clock
  # ---------------------------------------------------------------------------
  df.out <- df.out %>%
    left_join(x = ., y = df.time.frame, by = "id") %>%
    filter(delivered.unixts >= start.clock & delivered.unixts <= end.clock) %>%
    arrange(id, delivered.unixts)
  
  # ---------------------------------------------------------------------------
  # Decision rule: exclude EMAs that are "not valid"
  # ---------------------------------------------------------------------------
  df.out <- df.out %>% 
    mutate(Responded = as.character(Responded),
           Completed = as.character(Completed)) %>%
    mutate(Responded = if_else(Responded=="","Missing",Responded),
           Completed = if_else(Completed=="","Missing",Completed)) %>%
    filter((Responded=="True" & Completed=="True")|
             (Responded=="True" & Completed=="False")|
             (Responded=="True" & Completed=="Missing")|
             (Responded=="Missing" & Completed=="False")) %>%
    rename(responded=Responded,
           completed=Completed)
  
  # --------------------------------------------------------------------------- 
  # Decision rule: exclude EMAs based on difference between
  # assessment.unixts and delivered.unixts
  # ---------------------------------------------------------------------------
  df.out <- df.out %>% filter(is.na(delay)|(delay >= 0))
  
  return(df.out)
}


