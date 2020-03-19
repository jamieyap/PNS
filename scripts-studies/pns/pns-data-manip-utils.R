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


PNSCleanSmokingCount <- function(df){
  # About: Clean up raw responses to smoking quantity items in EMAs
  # Args: 
  #   df: ***one*** of post quit random, post quit urge,
  #     post quit about to slip, post quit about to slip part 2,
  #     post quit already slipped raw data;
  #     this data frame already has the three columns 
  #     smoking.indicator, smoking.timing, and smoking.qty
  # Output:
  #   dataset with cleaned up smoking item responses in two new
  #   columns: smoking.label and num.cigs.smoked
  
  # Check that inputs are what is expected ------------------------------------
  # (1) all rows of df may only be from one and only one assessment type 
  this.assessment.type <- unique(df$UB.type)
  assert_that(length(this.assessment.type)==1, msg = "Assessment types are mixed")
  # (2) df has columns we'll perform tasks on
  include.cols <- c("smoking.indicator", "smoking.timing", "smoking.qty",
                    "interval.duration.hours", "ema.id")
  check.cols <- (include.cols %in% colnames(df))
  assert_that(sum(!check.cols)==0, msg = "Missing column in df")
  
  # Begin ---------------------------------------------------------------------
  df$smoking.label <- NA
  df$num.cigs.smoked <- NA
  
  if(this.assessment.type == "Post-Quit Random"){
    
    # Post-Quit Random ##########################################################
    df$smoking.label <- replace(df$smoking.label, df$smoking.indicator == 0, "NO")
    df$smoking.label <- replace(df$smoking.label, df$smoking.indicator == 1 & df$smoking.qty == 0, "NO")
    df$smoking.label <- replace(df$smoking.label, (df$ema.id != 1) & (df$smoking.qty > 0), "YES")
    df$smoking.label <- replace(df$smoking.label, df$interval.duration.hours > 24, "UNKNOWN")
    # Those we have not labeled yet, label them as UNKNOWN
    df$smoking.label <- replace(df$smoking.label, is.na(df$smoking.label), "UNKNOWN") 
    
    # Fill in num.cigs.smoked
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 0, 0)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 1, 1)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 2, 2)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 3, 4)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 4, 6)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 5, 8)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 6, 10)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 7, 11)
    
  }else if(this.assessment.type == "Post-Quit Urge"){
    
    # Post-Quit Urge ############################################################
    df$smoking.label <- replace(df$smoking.label, df$smoking.indicator == 0, "NO")
    df$smoking.label <- replace(df$smoking.label, df$smoking.indicator == 1 & df$smoking.qty == 0, "NO")
    df$smoking.label <- replace(df$smoking.label, (df$ema.id != 1) & (df$smoking.qty > 0), "YES")
    df$smoking.label <- replace(df$smoking.label, df$interval.duration.hours > 24, "UNKNOWN")
    # Those we have not labeled yet, label them as UNKNOWN
    df$smoking.label <- replace(df$smoking.label, is.na(df$smoking.label), "UNKNOWN") 
    
    # Fill in num.cigs.smoked
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 0, 0)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 1, 1)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 2, 2)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 3, 4)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 4, 6)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 5, 8)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 6, 10)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 7, 11) 
    
  }else if(this.assessment.type == "Post-Quit About to Slip"){
    
    # Post-Quit About to Slip ###################################################
    df$smoking.label <- "UNKNOWN"
    df$num.cigs.smoked <- NA
    
  }else if(this.assessment.type == "Post-Quit About to Slip - Part2"){
    
    # Post-Quit About to Slip - Part2 ###########################################
    df$smoking.label <- replace(df$smoking.label, (df$ema.id != 1) & (df$smoking.qty > 0), "YES")
    df$smoking.label <- replace(df$smoking.label, df$smoking.qty == 0, "NO")
    # Those we have not labeled yet, label them as UNKNOWN
    df$smoking.label <- replace(df$smoking.label, is.na(df$smoking.label), "UNKNOWN")
    df$smoking.label <- replace(df$smoking.label, df$interval.duration.hours > 24, "UNKNOWN")
    
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    df$smoking.qty <- replace(df$smoking.qty, df$smoking.qty == 3, 2) 
    df$smoking.qty <- replace(df$smoking.qty, df$smoking.qty == 4, 3)
    df$smoking.qty <- replace(df$smoking.qty, df$smoking.qty == 5, 4)
    df$smoking.qty <- replace(df$smoking.qty, df$smoking.qty == 6, 5)
    df$smoking.qty <- replace(df$smoking.qty, df$smoking.qty == 7, 6)
    df$smoking.qty <- replace(df$smoking.qty, df$smoking.qty == 8, 7)
    
    # Fill in num.cigs.smoked
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 0, 0)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 1, 1)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 2, 2)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 3, 4)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 4, 6)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 5, 8)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 6, 10)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 7, 11)
    
  }else if(this.assessment.type == "Post-Quit Already Slipped"){
    
    # Post-Quit Already Slipped #################################################
    df$smoking.label <- replace(df$smoking.label, (df$ema.id != 1) & (df$smoking.qty > 0), "YES")
    df$smoking.label <- replace(df$smoking.label, df$smoking.qty == 0, "NO")
    # Those we have not labeled yet, label them as UNKNOWN
    df$smoking.label <- replace(df$smoking.label, is.na(df$smoking.label), "UNKNOWN") 
    df$smoking.label <- replace(df$smoking.label, df$interval.duration.hours > 24, "UNKNOWN")
    
    # Fill in num.cigs.smoked
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 0, 0)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 1, 1)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 2, 2)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 3, 4)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 4, 6)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 5, 8)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 6, 10)
    df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.qty == 7, 11)
    
  }else{
    stop("assessment type not valid")
  }
  
  ############### All EMA Types ###############################################
  # If label is NO, number.cigs.smoked=0
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "NO", 0)
  
  # If label is UNKNOWN, consider num.cigs.smoked missing
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "UNKNOWN", NA)
  
  return(df)
}

PNSCleanSmokingTime <- function(df){
  # About: Clean up raw responses to smoking time items in EMAs
  # Args: 
  #   df: ***one*** individual's data frame
  # Output:
  #   dataset with cleaned up smoking item responses in one new
  #   column: smoking.timing
  
  # Check that inputs are what is expected ------------------------------------
  # (1) all rows of df may only be from one and only one individual 
  this.id <- unique(df$id)
  assert_that(length(this.id)==1, msg = "Data from more than one individual")
  # (2) df has columns we'll perform tasks on
  include.cols <- c("interval.duration.hours", "ema.id")
  check.cols <- (include.cols %in% colnames(df))
  assert_that(sum(!check.cols)==0, msg = "Missing column in df")
  
  # Begin ---------------------------------------------------------------------
  # In PNS data, timing of smoking is reported in terms of time intervals
  # min.time = minimum duration of time between MOST RECENT cigarette and current assessment
  # max.time = maximum duration of time between MOST RECENT cigarette and current assessment
  df$min.time <- NA
  df$max.time <- NA
  
  # Responses to timing of smoking in Post-Quit Already slipped (0-8) is on a 
  # different scale than Post-Quit Random and Post-Quit Urge, both of 
  # which are on the same scale (1-9). We shift responses to Post-Quit Random and
  # Post-Quit Urge so that they are on a scale of 0-8
  df <- df %>% 
    mutate(smoking.timing = as.double(smoking.timing)) %>%
    mutate(smoking.timing = if_else(assessment.type == "Post-Quit Random", smoking.timing-1, smoking.timing)) %>%
    mutate(smoking.timing = if_else(assessment.type == "Post-Quit Urge", smoking.timing-1, smoking.timing))
  
  # Now that all responses to timing of smoking are on the same scale,
  # we will re-scale the responses to seconds from current assessment.
  # Possible values of min.time are then 0, 16, 31, 46, 61, 76, 91, 106, 121
  df <- df %>% mutate(min.time = if_else(smoking.timing != 0, (15*smoking.timing+1)*60, 0))
  
  # Check whether reported timing of smoking is not consistent with length of time interval
  df <- df %>% mutate(with.timing.conflict = if_else(df$min.time > df$interval.duration.secs, 1, 0))
  df$with.timing.conflict <- replace(df$with.timing.conflict, df$ema.id>1 & df$smoking.label=="UNKNOWN", NA)
  df$with.timing.conflict <- replace(df$with.timing.conflict, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours>24, NA)
  
  # Initialize max.time to be the beginning of the current interval
  df$max.time <- if_else(df$ema.id == 1,  
                         df$UB.ts - df$start.clock, 
                         df$UB.ts - df$LB.ts)
  
  ########### Additional criterion using info num.cigs.smoked #################
  
  # First, fill in num.cigs.smoked for first intervals that are UNKNOWN
  # and at most 24 hours in length
  
  # Fill in num.cigs.smoked
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 0, 0)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 1, 1)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 2, 2)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 3, 4)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 4, 6)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 5, 8)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 6, 10)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$ema.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 7, 11) 
  
  # If num.cigs.smoked==1 then we can obtain narrower YES intervals
  df$max.time <- if_else(df$num.cigs.smoked==1 & df$smoking.timing<8, (df$smoking.timing+1)*15*60,  df$max.time)
  df$max.time <- if_else(df$num.cigs.smoked==1 & df$smoking.timing==8 & df$ema.id>1,  df$UB.ts - df$LB.ts, df$max.time)
  df$max.time <- if_else(df$num.cigs.smoked==1 & df$smoking.timing==8 & df$ema.id==1,  df$UB.ts - df$start.clock, df$max.time)
  
  ############### What happens to max.time when with.timing.conflict == 1 #####
  df$max.time <- if_else(df$with.timing.conflict==1 & df$num.cigs.smoked>1,
                         24*60*60,
                         df$max.time)
  
  ############### Housekeeping ################################################
  
  # If min.time is missing then consider max.time missing as well
  df$max.time <- replace(df$max.time, is.na(df$min.time), NA)
  
  # If label is UNKNOWN, consider min.time and max.time as missing
  df$max.time <- replace(df$max.time, df$smoking.label == "UNKNOWN" & df$ema.id>1, NA)
  df$min.time <- replace(df$min.time, df$smoking.label == "UNKNOWN" & df$ema.id>1, NA)
  
  df$max.time <- replace(df$max.time, df$smoking.label == "UNKNOWN" & df$ema.id==1 & df$interval.duration.hours>24, NA)
  df$min.time <- replace(df$min.time, df$smoking.label == "UNKNOWN" & df$ema.id==1 & df$interval.duration.hours>24, NA)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "UNKNOWN" & df$ema.id==1 & df$interval.duration.hours>24, NA)
  
  # If no info provided to smoking.timing, set it equal to 99
  df$smoking.timing <- if_else(is.na(df$smoking.timing), 99, df$smoking.timing) 
  
  return(df)
}

PNSRefineSmokingTime <- function(df){
  # About: Use responses to smoking timing items in EMAs to refine 
  #        end points of reported time intervals with smoking
  # Args: 
  #   df: ***one*** individual's data frame
  # Output:
  #   dataset with narrower smoking intervals
  
  no.refinement.all.assessments <- (sum(!is.na(df$with.timing.conflict))==0)
  
  if(isTRUE(no.refinement.all.assessments)){
    # No refinement of labels of time intervals
    newdf <- df  %>% rename(interval.id = ema.id) %>%
      # Just take a subset of columns
      select(id, interval.id, 
             start.clock, end.clock, 
             LB.ts, UB.ts, 
             smoking.label, num.cigs.smoked)
  }else{
    # Refinement of intervals are possible
    n.assessments <- max(df$ema.id)
    newdf.list <-  list()
    
    for(j in 1:n.assessments){
      # Take subset of rows corresponding to assessment j
      newdf.this.assessment <- df %>% filter(ema.id == j) %>% 
        select(id, 
               start.clock, end.clock, 
               min.time, max.time, 
               interval.duration.secs, 
               LB.ts, UB.ts, 
               smoking.label, num.cigs.smoked, 
               with.timing.conflict)
      
      #########################################################################
      #########################################################################
      no.refinement.this.assessment <- (is.na(df[j,"with.timing.conflict"]))
      
      if(isTRUE(no.refinement.this.assessment)){
        # Append new info to current info
        newdf.this.assessment <- list(newdf.this.assessment)
        newdf.list <- append(newdf.list, newdf.this.assessment)
        # Do not proceed any further; move on to next assessment
        next
      }
      
      #########################################################################
      #########################################################################
      no.conflicting.info <- (df[j,"with.timing.conflict"] == 0)
      
      if(isTRUE(no.conflicting.info)){
        
        min.time <- newdf.this.assessment$min.time
        max.time <- newdf.this.assessment$max.time
        interval.duration.secs <- newdf.this.assessment$interval.duration.secs
        
        scenario.01 <- min.time>0 & max.time<interval.duration.secs
        scenario.02 <- min.time==0 & max.time<interval.duration.secs
        scenario.03 <- min.time>0 & max.time==interval.duration.secs
        scenario.04 <- min.time==0 & max.time==interval.duration.secs
        
        Clean01 <- function(newdf.this.assessment){
          newdf.this.assessment <- rbind(newdf.this.assessment,
                                         newdf.this.assessment,
                                         newdf.this.assessment)
          
          # First interval ----------------------------------------------------
          newdf.this.assessment[1,"LB.ts"] <- newdf.this.assessment[1,"LB.ts"] # No change 
          newdf.this.assessment[1,"UB.ts"] <- newdf.this.assessment[1,"UB.ts"] - newdf.this.assessment[1,"max.time"]
          newdf.this.assessment[1,"smoking.label"] <- "NO"
          newdf.this.assessment[1,"num.cigs.smoked"] <- 0
          
          # Second interval ---------------------------------------------------
          newdf.this.assessment[2,"LB.ts"] <- newdf.this.assessment[2,"UB.ts"] - newdf.this.assessment[2,"max.time"] 
          newdf.this.assessment[2,"UB.ts"] <- newdf.this.assessment[2,"UB.ts"] - newdf.this.assessment[2,"min.time"]
          newdf.this.assessment[2,"smoking.label"] <- "YES"
          newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"] # No change
          
          # Third interval ----------------------------------------------------
          newdf.this.assessment[3,"LB.ts"] <- newdf.this.assessment[3,"UB.ts"] - newdf.this.assessment[3,"min.time"] 
          newdf.this.assessment[3,"UB.ts"] <- newdf.this.assessment[3,"UB.ts"] # No change 
          newdf.this.assessment[3,"smoking.label"] <- "NO"
          newdf.this.assessment[3,"num.cigs.smoked"] <- 0
          
          return(newdf.this.assessment)
        }
        
        Clean02 <- function(newdf.this.assessment){
          newdf.this.assessment <- rbind(newdf.this.assessment,
                                         newdf.this.assessment)
          
          # First interval ----------------------------------------------------
          newdf.this.assessment[1,"LB.ts"] <- newdf.this.assessment[1,"LB.ts"] # No change 
          newdf.this.assessment[1,"UB.ts"] <- newdf.this.assessment[1,"UB.ts"] - newdf.this.assessment[1,"max.time"]
          newdf.this.assessment[1,"smoking.label"] <- "NO"
          newdf.this.assessment[1,"num.cigs.smoked"] <- 0
          
          # Second interval ---------------------------------------------------
          newdf.this.assessment[2,"LB.ts"] <- newdf.this.assessment[2,"UB.ts"] - newdf.this.assessment[2,"max.time"]
          newdf.this.assessment[2,"UB.ts"] <- newdf.this.assessment[2,"UB.ts"]
          newdf.this.assessment[2,"smoking.label"] <- "YES"
          newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"]  # No change 
          
          return(newdf.this.assessment)
        }
        
        Clean03 <- function(newdf.this.assessment){
          newdf.this.assessment <- rbind(newdf.this.assessment,
                                         newdf.this.assessment)
          
          # First interval ----------------------------------------------------
          newdf.this.assessment[1,"LB.ts"] <- newdf.this.assessment[1,"LB.ts"] # No change 
          newdf.this.assessment[1,"UB.ts"] <- newdf.this.assessment[1,"UB.ts"] - newdf.this.assessment[1,"min.time"]
          newdf.this.assessment[1,"smoking.label"] <- "YES"
          newdf.this.assessment[1,"num.cigs.smoked"] <- newdf.this.assessment[1,"num.cigs.smoked"]  # No change
          
          # Second interval ---------------------------------------------------
          newdf.this.assessment[2,"LB.ts"] <- newdf.this.assessment[2,"UB.ts"] - newdf.this.assessment[2,"min.time"]
          newdf.this.assessment[2,"UB.ts"] <- newdf.this.assessment[2,"UB.ts"]
          newdf.this.assessment[2,"smoking.label"] <- "NO"
          newdf.this.assessment[2,"num.cigs.smoked"] <- 0
          
          return(newdf.this.assessment)
        }
        
        if(scenario.01){
          newdf.this.assessment <- Clean01(newdf.this.assessment)
        }else if(scenario.02){
          newdf.this.assessment <- Clean02(newdf.this.assessment)
        }else if(scenario.03){
          newdf.this.assessment <- Clean03(newdf.this.assessment)
        }else{
          newdf.this.assessment <- newdf.this.assessment
        }
        
        # Append new info to current info
        newdf.this.assessment <- list(newdf.this.assessment)
        newdf.list <- append(newdf.list, newdf.this.assessment)
        next
      }
      
      with.conflicting.info <- (df[j,"with.timing.conflict"] == 1)
      if(with.conflicting.info){
        # Do not modify time interval
        newdf.this.assessment <- newdf.this.assessment
        prevdf.this.assessment <- NULL
        
        # Append new info to current info
        if(is.null(prevdf.this.assessment)){
          newdf.this.assessment <- list(newdf.this.assessment)
          newdf.list <- append(newdf.list, newdf.this.assessment)
        }else{
          newdf.list <- list(prevdf.this.assessment)
          newdf.this.assessment <- list(newdf.this.assessment)
          newdf.list <- append(newdf.list, newdf.this.assessment)
        }
      }
      
    } # End looping through all assessments
    
    newdf <- bind_rows(newdf.list)
    num.intervals <- nrow(newdf)
    newdf <- newdf %>% mutate(interval.id = 1:num.intervals) %>%
      select(id, interval.id, 
             start.clock, end.clock, 
             LB.ts, UB.ts, 
             smoking.label, num.cigs.smoked)
  } # End big IF-ELSE statement
  
  return(newdf)
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


