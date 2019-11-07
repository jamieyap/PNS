# About this script: Contains functions to perform data manipulation tasks

library(assertthat)
library(dplyr)

# -----------------------------------------------------------------------------
# SetClock() calculates start and end of clock for data analysis
# -----------------------------------------------------------------------------

SetClock <- function(df.quit.dates, study.duration, addtime){
  #
  # Requires libraries: assertthat, dplyr
  #   
  # Args: 
  # 
  #   (1) df.quit.dates (type:data.frame) has four columns named:
  #   username (col 1), quit.date (col 2), drop.date (col 3), 
  #   withdraw.date (col 4); dates (cols 2-4) are all in mm/dd/yyyy format
  #   (2) study.duration (type:numeric) is the duration of the study in days;
  #   must be positive
  #   (3) addtime (type:numeric) time difference in hours of timestamps created 
  #   on local machine and timestamps in raw data; can be positive or negative
  #
  # Output:
  # 
  #   return df.quit.dates (type:data frame) with two additional 
  #   columns (unix timestamps) named: (col 5) start.clock, (col 6) end.clock
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df.quit.dates), msg = "df.quit.dates IS NOT A DATA FRAME")
  assert_that(is.numeric(addtime), msg = "addtime MUST BE A NUMBER (POSITIVE, ZERO, OR NEGATIVE)")
  assert_that((is.numeric(study.duration)) & (study.duration>0), msg = "study.duration MUST BE A POSITIVE NUMBER")
  
  # Check: column names are correct
  assert_that(sum(!hasName(df.quit.dates, c("id","quit.date","drop.date","withdraw.date")))==0, msg = "COLUMN NAMES MUST BE (IN ORDER): id, quit.date, drop.date, withdraw.date")
  
  # Check: if dates exist then they must be in %m/%d/%Y format
  tmp.date.df <- as.character(df.quit.dates[,"quit.date"])
  tmp.date.df <- tmp.date.df[!((tmp.date.df=="")|is.na(tmp.date.df))]
  if(length(tmp.date.df)>0){
    assert_that(sum(is.na(strptime(tmp.date.df, format = "%m/%d/%Y")))==0, msg = "quit.date MUST BE IN mm/dd/yyyy FORMAT")
  }
  
  tmp.date.df <- as.character(df.quit.dates[,"drop.date"])
  tmp.date.df <- tmp.date.df[!((tmp.date.df=="")|is.na(tmp.date.df))]
  if(length(tmp.date.df)>0){
    assert_that(sum(is.na(strptime(tmp.date.df, format = "%m/%d/%Y")))==0, msg = "drop.date MUST BE IN mm/dd/yyyy FORMAT") 
  }
  
  tmp.date.df <- as.character(df.quit.dates[,"withdraw.date"])
  tmp.date.df <- tmp.date.df[!((tmp.date.df=="")|is.na(tmp.date.df))]
  if(length(tmp.date.df)>0){
    assert_that(sum(is.na(strptime(tmp.date.df, format = "%m/%d/%Y")))==0, msg = "withdraw.date MUST BE IN mm/dd/yyyy FORMAT")
  }
  
  # Format dates in cols 2-4
  df.quit.dates[,-1] <- apply(df.quit.dates[,-1], 2, strptime, format = "%m/%d/%Y")
  
  # Check: All participants to be included in the data analysis are expected to have quit dates
  assert_that(sum(is.na(df.quit.dates[,"quit.date"]))==0, msg = "ALL PARTICIPANTS INCLUDED IN DATA ANALYSIS MUST HAVE QUIT DATES")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  # Exclude participants who dropped or withdrew prior to quit date
  exclude.criteria <- (!is.na(df.quit.dates$drop.date) & (df.quit.dates$quit.date >= df.quit.dates$drop.date)) |
    (!is.na(df.quit.dates$withdraw.date) & (df.quit.dates$quit.date >= df.quit.dates$withdraw.date))
  
  df.quit.dates <- df.quit.dates[which(!exclude.criteria),]
  
  # Calculate timestamps corresponding to start and end of clock
  df.quit.dates$start.clock <- NA
  df.quit.dates$end.clock <- NA
  
  df.quit.dates[,"start.clock"] <- as.numeric(df.quit.dates[,"quit.date"]) + 4*60*60  # 4AM on Quit Day 
  df.quit.dates[,"end.clock"] <- df.quit.dates[,"start.clock"] + study.duration*24*60*60  - 4*60*60 # 12AM on D Days after Quit Day
  
  df.quit.dates[,"end.clock"] <- if_else(!is.na(df.quit.dates[,"withdraw.date"]), 
                                         as.numeric(df.quit.dates[,"withdraw.date"]), 
                                         df.quit.dates[,"end.clock"]) 
  
  df.quit.dates[,"start.clock"] <- df.quit.dates[,"start.clock"] + addtime*60*60 # Adjust for time zone differences
  df.quit.dates[,"end.clock"] <- df.quit.dates[,"end.clock"] + addtime*60*60 # Adjust for time zone differences
  
  # ---------------------------------------------------------------------------
  # Clean up output
  # ---------------------------------------------------------------------------
  
  df.quit.dates[,"quit.date"] <- strftime(df.quit.dates[,"quit.date"], format = "%m/%d/%Y")
  df.quit.dates[,"quit.date"] <- as.character(df.quit.dates[,"quit.date"])
  df.quit.dates <- df.quit.dates[, c("id","quit.date","start.clock","end.clock")]
  row.names(df.quit.dates) <- 1:nrow(df.quit.dates)
  
  return(df.quit.dates)
}

# -----------------------------------------------------------------------------
# Determine whether at least one question has been completed in an assessment
# -----------------------------------------------------------------------------

CheckAnyResponse <- function(df, drop.cols.index = NULL){
  # Requires libraries: assertthat, dplyr
  #   
  # Args:
  # 
  #   df: raw survey data
  #   drop.cols.index: column number of columns we will drop, e.g. c(1,2,3,4,5)
  #   
  # Output:
  #   
  #   return df with two new columns:
  #     with.any.response=1 if at least one question in an assessment was completed
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
  assert_that(is.null(drop.cols.index) | (is.numeric(drop.cols.index) & sum(drop.cols.index<=0)==0), 
              msg = "drop.cols.index must either be NULL or a list of positive numbers")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  if(!is.null(drop.cols.index)){
    # Keep only those rows corresponding to survey questions
    manipulated.df <- df %>% select(-drop.cols.index)
  }else{
    # Keep original data frame
    manipulated.df <- df
  }
  
  df[,"with.any.response"] <- manipulated.df %>% is.na(.) %>% as.data.frame(.) %>%
    mutate(count.with.missing.response = rowSums(.)) %>% 
    mutate(num.items = ncol(.)-1) %>%
    mutate(with.any.response = ifelse(count.with.missing.response==num.items, 0, 1)) %>%
    select(with.any.response)
  
  return(df)
}


# -----------------------------------------------------------------------------
# Creates an ID for each observation by group
# -----------------------------------------------------------------------------

CreateID <- function(dat, sequence.along, by.var, id.name){
  # About: CreateID() creates an ID for observations in sequence.along
  # and grouped according to by.var
  # 
  # Requires libraries: assertthat
  # 
  # Args:
  #   observations in dat should be ordered by increasing time
  # Output:
  #   a sequence of ids
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(dat), msg = "dat IS NOT A DATA FRAME")
  assert_that(is.string(sequence.along), msg = "sequence.along NOT A STRING")
  assert_that(is.string(by.var), msg = "by.var NOT A STRING")
  assert_that(is.string(id.name), msg = "id.name NOT A STRING")
  
  # Check: sequence.along and by.var belong to column names of dat
  assert_that(sequence.along %in% colnames(dat), msg = "sequence.along MUST BE COLUMNS OF dat")
  assert_that(by.var %in% colnames(dat), msg = "by.var MUST BE COLUMNS OF dat")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  my.formula <- as.formula(paste(sequence.along, "~", by.var))
  # example: group EMAs according to username
  #   aggregate(ema.type ~ username, data = ema.data, FUN=length)
  tmp <- aggregate(my.formula, data = dat, FUN=length)
  tmp$start <- 1
  tmp <- mapply(seq, from = tmp[,3], to = tmp[,2])
  tmp <- as.matrix(unlist(tmp))
  colnames(tmp) <- id.name
  
  return(tmp)
}

# -----------------------------------------------------------------------------
# Obtain past record of current value of a given column
# -----------------------------------------------------------------------------

GetPastRecords <- function(df.this.group, cols.today, h){
  # Requires libraries: assertthat
  # 
  # Args:
  # 
  #   df.this.group: data frame whose rows all belong to one group; 
  #   e.g. all rows belong to one participant
  #   cols.today: column names data of which we want to obtain past records of
  #   e.g. c("assessment.unixts","assessment.type")
  #   h: how far back we want to obtain records; can accomodate differing lengths of time
  #   e.g. c(1,3) if we want to obtain assessment time and assessment type from 1 or 3 records ago, respectively
  #   
  # Output:
  # 
  #   df.this.group with number of new columns equal to the length of h
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df.this.group), msg = "df.this.group IS NOT A DATA FRAME")
  assert_that(sum(!(cols.today %in% colnames(df.this.group)))==0, msg = "cols.today MUST BE COLUMNS IN df")
  assert_that(sum(!(is.numeric(h) & (h>0)))==0, msg = "INPUTS IN h MUST BE A POSITIVE NUMBER")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  for(i in 1:length(h)){
    df.this.group[, paste(cols.today[i], "_lag", h[i], sep="")] <- c(rep(NA,h[i]),head(df.this.group[, cols.today[i]], n=-h[i]))
  }
  
  return(df.this.group)
}

# -----------------------------------------------------------------------------
# Label time intervals according to whether have an indication of a participant
# smoking in a given period of time
# -----------------------------------------------------------------------------

# Functions for PNS study -----------------------------------------------------
SmokingLabelPNS01 <- function(df){
  # Note:
  # 
  #   SmokingLabelPNS01() provides labels of time intervals and number of 
  #   cigarettes smoked according to Method 01. Different labeling rules apply 
  #   to each EMA type.
  # 
  # Args:
  # 
  #   df is a data frame containing observations to be used for data analysis;
  #   observations not meeting inclusion criteria have been excluded from df
  # 
  # Output:
  # 
  #   df with new columns: smoking.label and num.cigs.smoked
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
  assert_that("record.status" %in% colnames(df), msg = "df MUST HAVE COLUMN record.status")
  assert_that("assessment.type" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.type")
  assert_that("assessment.id" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.id")
  assert_that("smoking.qty" %in% colnames(df), msg = "df MUST HAVE COLUMN smoking.qty")
  assert_that("interval.duration.hours" %in% colnames(df), msg = "df MUST HAVE COLUMN interval.duration.hours")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  df$smoking.label <- NA
  df$num.cigs.smoked <- NA
  
  # Post-Quit About to Slip ###################################################
  df$smoking.label <- replace(df$smoking.label, df$assessment.type == "Post-Quit About to Slip", "UNKNOWN")
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip", NA) 
  
  # Post-Quit About to Slip - Part2 ###########################################
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit About to Slip - Part2") & (df$assessment.id != 1) & (df$smoking.qty > 0), "YES")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit About to Slip - Part2") & (df$smoking.qty == 0), "NO")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit About to Slip - Part2") & is.na(df$smoking.label), "UNKNOWN") # Those we have not labeled yet, label them as UNKNOWN
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit About to Slip - Part2") & (df$interval.duration.hours > 24), "UNKNOWN")
  
  # Clean up responses so that smoking.qty for all EMA types below are on the same scale
  df$smoking.qty <- replace(df$smoking.qty, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 3, 2) 
  df$smoking.qty <- replace(df$smoking.qty, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 4, 3)
  df$smoking.qty <- replace(df$smoking.qty, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 5, 4)
  df$smoking.qty <- replace(df$smoking.qty, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 6, 5)
  df$smoking.qty <- replace(df$smoking.qty, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 7, 6)
  df$smoking.qty <- replace(df$smoking.qty, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 8, 7)
  
  # Fill in num.cigs.smoked
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 0, 0)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 1, 1)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 2, 2)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 3, 4)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 4, 6)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 5, 8)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 6, 10)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit About to Slip - Part2" & df$smoking.qty == 7, 11)
  
  # Post-Quit Already Slipped #################################################
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Already Slipped") & (df$assessment.id != 1) & (df$smoking.qty > 0), "YES")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Already Slipped") & (df$smoking.qty == 0), "NO")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Already Slipped") & is.na(df$smoking.label), "UNKNOWN") # Those we have not labeled yet, label them as UNKNOWN
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Already Slipped") & (df$interval.duration.hours > 24), "UNKNOWN")
  
  # Fill in num.cigs.smoked
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 0, 0)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 1, 1)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 2, 2)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 3, 4)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 4, 6)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 5, 8)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 6, 10)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Already Slipped" & df$smoking.qty == 7, 11)
  
  # Post-Quit Random ##########################################################
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Random") & (df$smoking.indicator == 0), "NO")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Random") & df$smoking.indicator == 1 & df$smoking.qty == 0, "NO")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Random") & (df$assessment.id != 1) & (df$smoking.qty > 0), "YES")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Random") & (df$interval.duration.hours > 24), "UNKNOWN")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Random") & is.na(df$smoking.label), "UNKNOWN") # Those we have not labeled yet, label them as UNKNOWN
  
  # Fill in num.cigs.smoked
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 0, 0)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 1, 1)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 2, 2)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 3, 4)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 4, 6)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 5, 8)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 6, 10)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Random" & df$smoking.qty == 7, 11)
  
  # Post-Quit Urge ############################################################
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Urge") & (df$smoking.indicator == 0), "NO")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Urge") & df$smoking.indicator == 1 & df$smoking.qty == 0, "NO")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Urge") & (df$assessment.id != 1) & (df$smoking.qty > 0), "YES")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Urge") & (df$interval.duration.hours > 24), "UNKNOWN")
  df$smoking.label <- replace(df$smoking.label, (df$assessment.type == "Post-Quit Urge") & is.na(df$smoking.label), "UNKNOWN") # Those we have not labeled yet, label them as UNKNOWN
  
  # Fill in num.cigs.smoked
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 0, 0)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 1, 1)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 2, 2)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 3, 4)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 4, 6)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 5, 8)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 6, 10)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.type == "Post-Quit Urge" & df$smoking.qty == 7, 11) 
  
  ############### All EMA Types ###############################################
  
  # If label is NO, number.cigs.smoked=0
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "NO", 0)
  
  # If label is UNKNOWN, consider num.cigs.smoked missing
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "UNKNOWN", NA)
  
  return(df)
}

SmokingTimingPNS <- function(df){
  # Note:
  # 
  #   Clean responses corresponding to reported timing of smoking
  #
  # Args:
  # 
  #   df is a data frame containing observations to be used for data analysis;
  #   observations not meeting inclusion criteria have been excluded from df
  #
  # Output:
  #
  #   df: new columns min.time, max.time, with.timing.conflict were added
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
  assert_that("assessment.type" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.type")
  assert_that("assessment.id" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.id")
  assert_that("assessment.unixts" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.unixts")
  assert_that("assessment.unixts_lag1" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.unixts_lag1")
  assert_that("smoking.timing" %in% colnames(df), msg = "df MUST HAVE COLUMN smoking.timing")
  assert_that("smoking.label" %in% colnames(df), msg = "df MUST HAVE COLUMN smoking.label")
  assert_that("num.cigs.smoked" %in% colnames(df), msg = "df MUST HAVE COLUMN num.cigs.smoked")
  assert_that("interval.duration.secs" %in% colnames(df), msg = "df MUST HAVE COLUMN interval.duration.secs")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
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
  df$with.timing.conflict <- replace(df$with.timing.conflict, df$assessment.id>1 & df$smoking.label=="UNKNOWN", NA)
  df$with.timing.conflict <- replace(df$with.timing.conflict, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours>24, NA)
  
  # Initialize max.time to be the beginning of the current interval
  df$max.time <- if_else(df$assessment.id == 1,  
                         df$assessment.unixts - df$start.clock, 
                         df$assessment.unixts - df$assessment.unixts_lag1)
  
  ########### Additional criterion using info num.cigs.smoked #################
  
  # First, fill in num.cigs.smoked for first intervals that are UNKNOWN
  # and at most 24 hours in length
  
  # Fill in num.cigs.smoked
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 0, 0)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 1, 1)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 2, 2)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 3, 4)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 4, 6)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 5, 8)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 6, 10)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$assessment.id==1 & df$smoking.label=="UNKNOWN" & df$interval.duration.hours <=24 & df$smoking.qty == 7, 11) 
  
  # If num.cigs.smoked==1 then we can obtain narrower YES intervals
  df$max.time <- if_else(df$num.cigs.smoked==1 & df$smoking.timing<8, (df$smoking.timing+1)*15*60,  df$max.time)
  df$max.time <- if_else(df$num.cigs.smoked==1 & df$smoking.timing==8 & df$assessment.id>1,  df$assessment.unixts - df$assessment.unixts_lag1, df$max.time)
  df$max.time <- if_else(df$num.cigs.smoked==1 & df$smoking.timing==8 & df$assessment.id==1,  df$assessment.unixts - df$start.clock, df$max.time)
  
  ############### What happens to max.time when with.timing.conflict == 1 #####
  df$max.time <- if_else(df$with.timing.conflict==1 & df$num.cigs.smoked>1,
                         24*60*60,
                         df$max.time)
  
  ############### Housekeeping ################################################
  
  # If min.time is missing then consider max.time missing as well
  df$max.time <- replace(df$max.time, is.na(df$min.time), NA)
  
  # If label is UNKNOWN, consider min.time and max.time as missing
  df$max.time <- replace(df$max.time, df$smoking.label == "UNKNOWN" & df$assessment.id>1, NA)
  df$min.time <- replace(df$min.time, df$smoking.label == "UNKNOWN" & df$assessment.id>1, NA)
  
  df$max.time <- replace(df$max.time, df$smoking.label == "UNKNOWN" & df$assessment.id==1 & df$interval.duration.hours>24, NA)
  df$min.time <- replace(df$min.time, df$smoking.label == "UNKNOWN" & df$assessment.id==1 & df$interval.duration.hours>24, NA)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "UNKNOWN" & df$assessment.id==1 & df$interval.duration.hours>24, NA)
  
  # If no info provided to smoking.timing, set it equal to 99
  df$smoking.timing <- if_else(is.na(df$smoking.timing), 99, df$smoking.timing) 
  
  return(df)
}

SmokingRelabel <- function(df, FUN_RESOLVE_CONFLICT = NULL){
  # Note: df is a data frame corresponding to only ONE participant
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------  
  assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
  assert_that("max.time" %in% colnames(df), msg = "df MUST HAVE COLUMN max.time")
  assert_that("min.time" %in% colnames(df), msg = "df MUST HAVE COLUMN min.time")
  assert_that("smoking.label" %in% colnames(df), msg = "df MUST HAVE COLUMN smoking.label")
  assert_that("num.cigs.smoked" %in% colnames(df), msg = "df MUST HAVE COLUMN num.cigs.smoked")
  assert_that("smoking.timing" %in% colnames(df), msg = "df MUST HAVE COLUMN smoking.timing")
  assert_that("with.timing.conflict" %in% colnames(df), msg = "df MUST HAVE COLUMN with.timing.conflict")
  assert_that("interval.duration.secs" %in% colnames(df), msg = "df MUST HAVE COLUMN interval.duration.secs")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------  
  
  # Take subset of columns of df
  
  df <- df %>% select(id, assessment.id, 
                      start.clock, end.clock, 
                      assessment.unixts, assessment.unixts_lag1, 
                      smoking.label, num.cigs.smoked, 
                      min.time, max.time, smoking.timing,
                      with.timing.conflict, interval.duration.secs)
  df$LB <- if_else(df$assessment.id==1, df$start.clock, df$assessment.unixts_lag1)
  df$UB <- df$assessment.unixts
  
  if(sum(!is.na(df$with.timing.conflict))==0){
    # No refinement of labels of time intervals
    newdf <- df  %>% mutate(interval.id = assessment.id) %>%
      select(id, interval.id, start.clock, end.clock, LB, UB, smoking.label, num.cigs.smoked)
  }else{# Refinement of intervals are possible
    n.assessments <- max(df$assessment.id)
    newdf.list <-  list()
    
    for(j in 1:n.assessments){
      # Take subset of rows corresponding to assessment j
      newdf.this.assessment <- df %>% filter(assessment.id == j) %>% 
        select(id, start.clock, end.clock, min.time, max.time, smoking.timing, interval.duration.secs, LB, UB, smoking.label, num.cigs.smoked, with.timing.conflict)
      
      #########################################################################
      #########################################################################
      
      if(is.na(df[j,"with.timing.conflict"])){
        # Append new info to current info
        newdf.this.assessment <- list(newdf.this.assessment)
        newdf.list <- append(newdf.list, newdf.this.assessment)
        next
      }
      
      #########################################################################
      #########################################################################
      
      if(df[j,"with.timing.conflict"] == 0){
        
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
          newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"LB"] # No change 
          newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"max.time"]
          newdf.this.assessment[1,"smoking.label"] <- "NO"
          newdf.this.assessment[1,"num.cigs.smoked"] <- 0
          
          # Second interval ---------------------------------------------------
          newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"max.time"] 
          newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"min.time"]
          newdf.this.assessment[2,"smoking.label"] <- "YES"
          newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"] # No change
          
          # Third interval ----------------------------------------------------
          newdf.this.assessment[3,"LB"] <- newdf.this.assessment[3,"UB"] - newdf.this.assessment[3,"min.time"] 
          newdf.this.assessment[3,"UB"] <- newdf.this.assessment[3,"UB"] # No change 
          newdf.this.assessment[3,"smoking.label"] <- "NO"
          newdf.this.assessment[3,"num.cigs.smoked"] <- 0
          
          return(newdf.this.assessment)
        }
        
        Clean02 <- function(newdf.this.assessment){
          newdf.this.assessment <- rbind(newdf.this.assessment,
                                         newdf.this.assessment)
          
          # First interval ----------------------------------------------------
          newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"LB"] # No change 
          newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"max.time"]
          newdf.this.assessment[1,"smoking.label"] <- "NO"
          newdf.this.assessment[1,"num.cigs.smoked"] <- 0
          
          # Second interval ---------------------------------------------------
          newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"max.time"]
          newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"]
          newdf.this.assessment[2,"smoking.label"] <- "YES"
          newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"]  # No change 
          
          return(newdf.this.assessment)
        }
        
        Clean03 <- function(newdf.this.assessment){
          newdf.this.assessment <- rbind(newdf.this.assessment,
                                         newdf.this.assessment)
          
          # First interval ----------------------------------------------------
          newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"LB"] # No change 
          newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"min.time"]
          newdf.this.assessment[1,"smoking.label"] <- "YES"
          newdf.this.assessment[1,"num.cigs.smoked"] <- newdf.this.assessment[1,"num.cigs.smoked"]  # No change
          
          # Second interval ---------------------------------------------------
          newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"min.time"]
          newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"]
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
      
      #########################################################################
      #########################################################################
      
      if(df[j,"with.timing.conflict"] == 1){
        
        if(is.null(FUN_RESOLVE_CONFLICT)){
          # Do not modify time interval
          newdf.this.assessment <- newdf.this.assessment
          prevdf.this.assessment <- NULL
        }else{
          out.this.assessment <- FUN_RESOLVE_CONFLICT(newdf.list = newdf.list, newdf.this.assessment = newdf.this.assessment)
          newdf.this.assessment <- out.this.assessment$newdf.this.assessment
          prevdf.this.assessment <- out.this.assessment$prevdf.this.assessment
          reset.list <- out.this.assessment$reset.list
          if(reset.list){
            newdf.list <- list()
          }
        }
        
        # Append new info to current info
        if(is.null(prevdf.this.assessment)){
          newdf.this.assessment <- list(newdf.this.assessment)
          newdf.list <- append(newdf.list, newdf.this.assessment)
        }else{
          newdf.list <- list(prevdf.this.assessment)
          newdf.this.assessment <- list(newdf.this.assessment)
          newdf.list <- append(newdf.list, newdf.this.assessment)
        }
        
      }  # End case: df[j,"with.timing.conflict"] == 1
      
    } # END LOOPING THROUGH ALL ASSESSMENTS
    
    newdf <- bind_rows(newdf.list)
    num.intervals <- nrow(newdf)
    newdf <- newdf %>% mutate(interval.id = 1:num.intervals) %>%
      select(id, interval.id, start.clock, end.clock, LB, UB, smoking.label, num.cigs.smoked)
  } # END BIG IF-ELSE STATEMENT
  
  return(newdf)
}

ResolveConflict <- function(newdf.list, newdf.this.assessment){
  
  reset.list <- FALSE
  prevdf.this.assessment <- NULL
  
  lookup.table <- bind_rows(newdf.list)
  
  if(nrow(lookup.table) == 0){
    
    newdf.this.assessment <- as.data.frame(newdf.this.assessment)
    
  }else if(nrow(lookup.table) == 1 & lookup.table[1,"smoking.label"]=="UNKNOWN"){
    reset.list <- TRUE
    # Note: At assessment.id=0, it is always the case that either with.timing.conflict=0 or NA
    if(newdf.this.assessment["UB"] - newdf.this.assessment["max.time"] <= newdf.this.assessment["start.clock"] &
       newdf.this.assessment["UB"] - newdf.this.assessment["min.time"] <= newdf.this.assessment["start.clock"]){
      
      newdf.this.assessment <- newdf.this.assessment
      
      newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"start.clock"]
      newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"]
      newdf.this.assessment[1,"smoking.label"] <- "UNKNOWN"
      newdf.this.assessment[1,"num.cigs.smoked"] <- NA
      
    }else if(newdf.this.assessment["UB"] - newdf.this.assessment["max.time"] <= newdf.this.assessment["start.clock"] &
             newdf.this.assessment["UB"] - newdf.this.assessment["min.time"] > newdf.this.assessment["start.clock"]){
      
      newdf.this.assessment <- rbind(newdf.this.assessment,
                                     newdf.this.assessment)
      
      newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"start.clock"]
      newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"min.time"]
      newdf.this.assessment[1,"smoking.label"] <- "UNKNOWN"
      newdf.this.assessment[1,"num.cigs.smoked"] <- NA
      
      newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"min.time"]
      newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"]
      newdf.this.assessment[1,"smoking.label"] <- "NO"
      newdf.this.assessment[1,"num.cigs.smoked"] <- 0
      
    }else if(newdf.this.assessment["UB"] - newdf.this.assessment["max.time"] > newdf.this.assessment["start.clock"] &
             newdf.this.assessment["UB"] - newdf.this.assessment["min.time"] > newdf.this.assessment["start.clock"] &
             newdf.this.assessment["min.time"] > 0){
      
      newdf.this.assessment <- rbind(newdf.this.assessment,
                                     newdf.this.assessment,
                                     newdf.this.assessment)
      
      newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"start.clock"]
      newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"max.time"]
      newdf.this.assessment[1,"smoking.label"] <- "NO"
      newdf.this.assessment[1,"num.cigs.smoked"] <- 0
      
      newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"max.time"]
      newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"min.time"]
      newdf.this.assessment[2,"smoking.label"] <- "YES"
      newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"]
      
      newdf.this.assessment[3,"LB"] <- newdf.this.assessment[3,"UB"] - newdf.this.assessment[3,"min.time"]
      newdf.this.assessment[3,"UB"] <- newdf.this.assessment[3,"UB"]
      newdf.this.assessment[3,"smoking.label"] <- "NO"
      newdf.this.assessment[3,"num.cigs.smoked"] <- 0
      
    }else{
      
      newdf.this.assessment <- rbind(newdf.this.assessment,
                                     newdf.this.assessment)
      
      newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"start.clock"]
      newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"max.time"]
      newdf.this.assessment[1,"smoking.label"] <- "NO"
      newdf.this.assessment[1,"num.cigs.smoked"] <- 0
      
      newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"max.time"]
      newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"]
      newdf.this.assessment[2,"smoking.label"] <- "YES"
      newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"]
      
    }
  }else if(nrow(lookup.table) == 1 & lookup.table[1,"smoking.label"]!="UNKNOWN"){
    
    newdf.this.assessment <- newdf.this.assessment
    
  }else if(nrow(lookup.table) > 1){
    
    if(newdf.this.assessment["smoking.timing"]==8){
      newdf.this.assessment["max.time"] <- newdf.this.assessment["UB"] - 24*60*60
    }
    
    
    idx.lower <- 1
    idx.upper <- 1
    
    idx.upper.conditions <- which(lookup.table[,"UB"] > as.numeric(newdf.this.assessment["UB"] - newdf.this.assessment["min.time"]))
    if(length(idx.upper.conditions)>0){
      idx.upper <- as.numeric(min(idx.upper.conditions))
    }
    
    if(newdf.this.assessment["UB"] - newdf.this.assessment["max.time"] > newdf.this.assessment["start.clock"]){
      idx.lower.conditions <- which(lookup.table[,"UB"] < as.numeric(newdf.this.assessment["UB"] - newdf.this.assessment["max.time"]))
      if(length(idx.lower.conditions)>0){
        idx.lower <- as.numeric(max(idx.lower.conditions)+1)
      }
    }
    
    subset.lookup.table <- lookup.table[idx.lower:idx.upper,]
    # Check whether any time interval in lookup.table is either a YES or NO interval
    # If so, do not proceed in calculations
    if(sum(subset.lookup.table[,"smoking.label"]!="UNKNOWN")>0){
      newdf.this.assessment <- newdf.this.assessment
    }else{
      
      if(idx.lower==1 & newdf.this.assessment["UB"]-newdf.this.assessment["max.time"]>newdf.this.assessment["start.clock"]){
        
        reset.list <- TRUE
        
        newdf.this.assessment <- rbind(newdf.this.assessment,
                                       newdf.this.assessment,
                                       newdf.this.assessment)
        
        newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"start.clock"]
        newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"max.time"]
        newdf.this.assessment[1,"smoking.label"] <- "NO"
        newdf.this.assessment[1,"num.cigs.smoked"] <- 0
        
        newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"max.time"]
        newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"min.time"]
        newdf.this.assessment[2,"smoking.label"] <- "YES"
        newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"]
        
        newdf.this.assessment[3,"LB"] <- newdf.this.assessment[3,"UB"] - newdf.this.assessment[3,"min.time"]
        newdf.this.assessment[3,"UB"] <- newdf.this.assessment[3,"UB"]
        newdf.this.assessment[3,"smoking.label"] <- "NO"
        newdf.this.assessment[3,"num.cigs.smoked"] <- 0
        
      }else if(idx.lower==1 & newdf.this.assessment["UB"]-newdf.this.assessment["max.time"]<=newdf.this.assessment["start.clock"]){
        
        reset.list <- TRUE
        
        newdf.this.assessment <- rbind(newdf.this.assessment,
                                       newdf.this.assessment)
        
        newdf.this.assessment[1,"LB"] <- newdf.this.assessment[1,"start.clock"]
        newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"min.time"]
        newdf.this.assessment[1,"smoking.label"] <- "YES"
        newdf.this.assessment[1,"num.cigs.smoked"] <- newdf.this.assessment[1,"num.cigs.smoked"]
        
        newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"min.time"]
        newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"]
        newdf.this.assessment[2,"smoking.label"] <- "NO"
        newdf.this.assessment[2,"num.cigs.smoked"] <- 0
        
      }else if(idx.lower>1 & newdf.this.assessment["min.time"]>0){  # idx.lower>1
        
        newdf.this.assessment <- rbind(newdf.this.assessment,
                                       newdf.this.assessment,
                                       newdf.this.assessment)
        
        newdf.this.assessment[1,"LB"] <- subset.lookup.table[1,"LB"]
        newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"max.time"] 
        newdf.this.assessment[1,"smoking.label"] <- "NO"
        newdf.this.assessment[1,"num.cigs.smoked"] <- 0
        
        newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"max.time"]
        newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"min.time"]
        newdf.this.assessment[2,"smoking.label"] <- "YES"
        newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"]
        
        newdf.this.assessment[3,"LB"] <- newdf.this.assessment[3,"UB"] - newdf.this.assessment[3,"min.time"]
        newdf.this.assessment[3,"UB"] <- newdf.this.assessment[3,"UB"]
        newdf.this.assessment[3,"smoking.label"] <- "NO"
        newdf.this.assessment[3,"num.cigs.smoked"] <- 0
        
        prevdf.this.assessment <- lookup.table[1:(idx.lower-1),]
        reset.list <- TRUE
        
      }else{ # idx.lower>1 & newdf.this.assessment["min.time"]==0
        
        newdf.this.assessment <- rbind(newdf.this.assessment,
                                       newdf.this.assessment)
        
        newdf.this.assessment[1,"LB"] <- subset.lookup.table[1,"LB"]
        newdf.this.assessment[1,"UB"] <- newdf.this.assessment[1,"UB"] - newdf.this.assessment[1,"max.time"] 
        newdf.this.assessment[1,"smoking.label"] <- "NO"
        newdf.this.assessment[1,"num.cigs.smoked"] <- 0
        
        newdf.this.assessment[2,"LB"] <- newdf.this.assessment[2,"UB"] - newdf.this.assessment[2,"max.time"]
        newdf.this.assessment[2,"UB"] <- newdf.this.assessment[2,"UB"]
        newdf.this.assessment[2,"smoking.label"] <- "YES"
        newdf.this.assessment[2,"num.cigs.smoked"] <- newdf.this.assessment[2,"num.cigs.smoked"]
        
        prevdf.this.assessment <- lookup.table[1:(idx.lower-1),]
        reset.list <- TRUE
        
      }
    }
  }else{
    
    newdf.this.assessment <- newdf.this.assessment
    
  }
  
  return(list(newdf.this.assessment = newdf.this.assessment, reset.list = reset.list, prevdf.this.assessment = prevdf.this.assessment))
}

CheckWithin <- function(ref.df, ref.this.var, df, study){
  
  if(study == "PNS"){
    
    # ---------------------------------------------------------------------------
    # Check validity of inputs
    # ---------------------------------------------------------------------------
    
    # Check: inputs are of the correct type
    assert_that(is.data.frame(ref.df), msg = "ref.df IS NOT A DATA FRAME")
    assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
    assert_that("LB.seconds" %in% colnames(df), msg = "LB.seconds must be the name of a column in df")
    assert_that("UB.seconds" %in% colnames(df), msg = "UB.seconds must be the name of a column in df")
    assert_that(ref.this.var %in% colnames(ref.df), msg = "ref.this.var must be the name of a column in ref.df")
    assert_that("engaged.yes" %in% colnames(ref.df), msg = "engaged.yes must be the name of a column in ref.df")
    assert_that("assessment.type" %in% colnames(ref.df), msg = "assessment.type must be the name of a column in ref.df")
    
    # Only Post-Quit Random EMAs
    these.times <- ref.df %>% filter(engaged.yes==0 & assessment.type == "Post-Quit Random") %>% select(ref.this.var)
    
    if(length(as.numeric(unlist(these.times))) == 0){
      df[,"count.random.ema.not.engaged"] <- 0
    }else{
      for(j in 1:nrow(df)){
        df[j,"count.random.ema.not.engaged"] <- sum((these.times > df[j,"LB.seconds"]) & (these.times < df[j,"UB.seconds"]))
      }
    }
    
    # Only Post-Quit Random EMAs
    these.times <- ref.df %>% filter(with.any.response==0 & assessment.type == "Post-Quit Random") %>% select(ref.this.var)
    
    if(length(as.numeric(unlist(these.times))) == 0){
      df[,"count.random.ema.no.response"] <- 0
    }else{
      for(j in 1:nrow(df)){
        df[j,"count.random.ema.no.response"] <- sum((these.times > df[j,"LB.seconds"]) & (these.times < df[j,"UB.seconds"]))
      }
    }
    
  }else if(study == "BreakFree"){
    
    # ---------------------------------------------------------------------------
    # Check validity of inputs
    # ---------------------------------------------------------------------------
    
    # Check: inputs are of the correct type
    assert_that(is.data.frame(ref.df), msg = "ref.df IS NOT A DATA FRAME")
    assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
    assert_that("LB.seconds" %in% colnames(df), msg = "LB.seconds must be the name of a column in df")
    assert_that("UB.seconds" %in% colnames(df), msg = "UB.seconds must be the name of a column in df")
    assert_that(ref.this.var %in% colnames(ref.df), msg = "ref.this.var must be the name of a column in ref.df")
    assert_that("with.any.response" %in% colnames(ref.df), msg = "engaged.yes must be the name of a column in ref.df")
    assert_that("assessment.type" %in% colnames(ref.df), msg = "assessment.type must be the name of a column in ref.df")
    
    # Only Random EMAs
    these.times <- ref.df %>% filter(with.any.response==0 & assessment.type == "RandomEMA") %>% select(ref.this.var)
    
    if(length(as.numeric(unlist(these.times))) == 0){
      df[,"count.random.ema.no.response"] <- 0
    }else{
      for(j in 1:nrow(df)){
        df[j,"count.random.ema.no.response"] <- sum((these.times > df[j,"LB.seconds"]) & (these.times < df[j,"UB.seconds"]))
      }
    }
    
    
  }else{
    
    print("Please enter a valid study. Choices are: PNS, BreakFree")
     
  }
  
  return(df)
}

SmokingLabelBreakFree01 <- function(df){
  # Note:
  # 
  #   SmokingLabelBreakFree01() provides labels of time intervals and number of 
  #   cigarettes smoked according to Method 01.
  # 
  # Args:
  # 
  #   df is a data frame containing observations to be used for data analysis;
  #   observations not meeting inclusion criteria have been excluded from df
  # 
  # Output:
  # 
  #   df with new columns: smoking.label and num.cigs.smoked
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
  assert_that("record.status" %in% colnames(df), msg = "df MUST HAVE COLUMN record.status")
  assert_that("assessment.type" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.type")
  assert_that("assessment.id" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.id")
  assert_that("interval.duration.hours" %in% colnames(df), msg = "df MUST HAVE COLUMN interval.duration.hours")
  assert_that("Smoked.Cig." %in% colnames(df), msg = "df MUST HAVE COLUMN Smoked.Cig.")
  assert_that("Howmany.Cigs." %in% colnames(df), msg = "df MUST HAVE COLUMN Howmany.Cigs.")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  df$smoking.label <- NA
  df$num.cigs.smoked <- NA
  
  df <- df %>% mutate(smoking.label = as.character(Smoked.Cig.))
  df$smoking.label <- replace(df$smoking.label, df$smoking.label=="Yes", "YES")
  df$smoking.label <- replace(df$smoking.label, df$smoking.label=="No", "NO")
  df$smoking.label <- replace(df$smoking.label, is.na(df$smoking.label), "UNKNOWN")
  df$smoking.label <- replace(df$smoking.label, df$smoking.label!="UNKNOWN" & df$interval.duration.hours>24, "UNKNOWN")
  
  df <- df %>%
    mutate(num.cigs.smoked = as.character(Howmany.Cigs.)) %>%
    mutate(num.cigs.smoked = as.numeric(num.cigs.smoked)) %>%
    mutate(num.cigs.smoked = replace(num.cigs.smoked, smoking.label=="NO", 0))
  
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "UNKNOWN" & df$assessment.id>1, NA)
  df$num.cigs.smoked <- replace(df$num.cigs.smoked, df$smoking.label == "UNKNOWN" & df$assessment.id==1 & df$interval.duration.hours>24, NA)
  
  
  return(df)
}

SmokingTimingBreakFree <- function(df){
  # Note:
  # 
  #   Clean responses corresponding to reported timing of smoking
  #
  # Args:
  # 
  #   df is a data frame containing observations to be used for data analysis;
  #   observations not meeting inclusion criteria have been excluded from df
  #
  # Output:
  #
  #   df: new columns min.time, max.time, with.timing.conflict were added
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
  assert_that("assessment.type" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.type")
  assert_that("assessment.id" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.id")
  assert_that("assessment.unixts" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.unixts")
  assert_that("assessment.unixts_lag1" %in% colnames(df), msg = "df MUST HAVE COLUMN assessment.unixts_lag1")
  assert_that("How.long.ago." %in% colnames(df), msg = "df MUST HAVE COLUMN How.long.ago.")
  assert_that("Recent.cig.how.long.ago." %in% colnames(df), msg = "df MUST HAVE COLUMN Recent.cig.how.long.ago.")
  assert_that("How.long.ago.smoke.first.cig" %in% colnames(df), msg = "df MUST HAVE COLUMN How.long.ago.smoke.first.cig")
  assert_that("smoking.label" %in% colnames(df), msg = "df MUST HAVE COLUMN smoking.label")
  assert_that("num.cigs.smoked" %in% colnames(df), msg = "df MUST HAVE COLUMN num.cigs.smoked")
  assert_that("interval.duration.secs" %in% colnames(df), msg = "df MUST HAVE COLUMN interval.duration.secs")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  # Timing of smoking is reported in terms of time intervals
  # min.time = minimum duration of time between MOST RECENT cigarette and current assessment
  # max.time = maximum duration of time between FIRST cigarette (if applicable) and current assessment
  df$min.time <- NA_real_
  df$max.time <- NA_real_
  
  # If max time corresponds to "More than 12 hrs" we will set smoking.timing=8
  # so that we can use the SmokingRelabel() function
  df$smoking.timing <- 99
  
  # Clean up responses
  df$How.long.ago. <- as.character(df$How.long.ago.)
  df$Recent.cig.how.long.ago. <- as.character(df$Recent.cig.how.long.ago.)
  df$How.long.ago.smoke.first.cig <- as.character(df$How.long.ago.smoke.first.cig)
  
  #############################################################################
  # Determine timing of smoking when individual smoked 1 cigarette
  #############################################################################
  
  df <- df %>%
    # min.time: Individual smoked 1 cigarette
    mutate(min.time = if_else(How.long.ago. == "0 - 2 hrs", 0, min.time)) %>%
    mutate(min.time = if_else(How.long.ago. == "2 hrs - 4 hrs", 2, min.time)) %>%
    mutate(min.time = if_else(How.long.ago. == "4 hrs - 6 hrs", 4, min.time)) %>%
    mutate(min.time = if_else(How.long.ago. == "6 hrs - 8 hrs", 6, min.time)) %>%
    mutate(min.time = if_else(How.long.ago. == "8 hrs - 10 hrs", 8, min.time)) %>%
    mutate(min.time = if_else(How.long.ago. == "10 hrs - 12 hrs", 10, min.time)) %>%
    mutate(min.time = if_else(How.long.ago. == "More than 12 hrs", 12, min.time)) %>%
    # max.time: Individual smoked 1 cigarette
    mutate(max.time = if_else(How.long.ago. == "0 - 2 hrs", 2, max.time)) %>%
    mutate(max.time = if_else(How.long.ago. == "2 hrs - 4 hrs", 4, max.time)) %>%
    mutate(max.time = if_else(How.long.ago. == "4 hrs - 6 hrs", 6, max.time)) %>%
    mutate(max.time = if_else(How.long.ago. == "6 hrs - 8 hrs", 8, max.time)) %>%
    mutate(max.time = if_else(How.long.ago. == "8 hrs - 10 hrs", 10, max.time)) %>%
    mutate(max.time = if_else(How.long.ago. == "10 hrs - 12 hrs", 12, max.time)) %>%
    mutate(max.time = if_else(How.long.ago. == "More than 12 hrs", NA_real_, max.time)) %>%
    # Convert hours to seconds
    mutate(min.time = if_else(!is.na(How.long.ago.), min.time*60*60, min.time)) %>% 
    mutate(max.time = if_else(!is.na(How.long.ago.), max.time*60*60, max.time)) %>%
    # Max.time when response is "More than 12 hrs"
    mutate(max.time = if_else(How.long.ago. == "More than 12 hrs", interval.duration.secs, max.time))
  
  # So that we can apply the SmokingRelabel() function to the Break free study data
  df$smoking.timing <- replace(df$smoking.timing, df$How.long.ago. == "More than 12 hrs", 8)
  
  #############################################################################
  # Determine timing of smoking when individual smoked more than 1 cigarette
  #############################################################################
  
  df <- df %>%
    # min.time: Individual smoked more than 1 cigarette
    mutate(min.time = if_else(Recent.cig.how.long.ago. == "0 - 2 hrs", 0, min.time)) %>%
    mutate(min.time = if_else(Recent.cig.how.long.ago. == "2 hrs - 4 hrs", 2, min.time)) %>%
    mutate(min.time = if_else(Recent.cig.how.long.ago. == "4 hrs - 6 hrs", 4, min.time)) %>%
    mutate(min.time = if_else(Recent.cig.how.long.ago. == "6 hrs - 8 hrs", 6, min.time)) %>%
    mutate(min.time = if_else(Recent.cig.how.long.ago. == "8 hrs - 10 hrs", 8, min.time)) %>%
    mutate(min.time = if_else(Recent.cig.how.long.ago. == "10 hrs - 12 hrs", 10, min.time)) %>%
    mutate(min.time = if_else(Recent.cig.how.long.ago. == "More than 12 hrs", 12, min.time)) %>%
    # max.time: Individual smoked more than 1 cigarette
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "0 - 2 hrs", 2, max.time)) %>%
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "2 hrs - 4 hrs", 4, max.time)) %>%
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "4 hrs - 6 hrs", 6, max.time)) %>%
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "6 hrs - 8 hrs", 8, max.time)) %>%
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "8 hrs - 10 hrs", 10, max.time)) %>%
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "10 hrs - 12 hrs", 12, max.time)) %>%
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "More than 12 hrs", NA_real_, max.time)) %>%
    # Convert hours to seconds
    mutate(min.time = if_else(!is.na(Recent.cig.how.long.ago.), min.time*60*60, min.time)) %>% 
    mutate(max.time = if_else(!is.na(How.long.ago.smoke.first.cig), max.time*60*60, max.time)) %>%
    # Max.time when response is "More than 12 hrs"
    mutate(max.time = if_else(How.long.ago.smoke.first.cig == "More than 12 hrs", interval.duration.secs, max.time)) 
  
  # So that we can apply the SmokingRelabel() function to the Break free study data
  df$smoking.timing <- replace(df$smoking.timing, df$How.long.ago.smoke.first.cig == "More than 12 hrs", 8)
  
  #############################################################################
  # Take care of special cases
  #############################################################################
  
  df <- df %>%
    # Deal with intervals having assessment.id==1
    mutate(max.time = if_else(assessment.id == 1 & max.time > interval.duration.secs, interval.duration.secs, max.time)) %>%
    # Deal with intervals having assessment.id!=1
    mutate(max.time = if_else(assessment.id!=1 & max.time > interval.duration.secs & min.time < interval.duration.secs, interval.duration.secs, max.time))
  
  #############################################################################
  # Check whether reported timing of smoking is not consistent with length of 
  # time interval or whether min.time is greater than max.time
  #############################################################################
  
  df <- df %>% 
    mutate(with.timing.conflict = if_else((min.time >= interval.duration.secs) | (min.time >= max.time), 1, 0)) %>%
    # Do not assess with.timing.conflict when interval is UNKNOWN when assessment.id>1
    mutate(with.timing.conflict = replace(with.timing.conflict, assessment.id>1 & smoking.label=="UNKNOWN", NA_real_)) %>%
    # Do not assess with.timing.conflict when interval is UNKNOWN and length of interval is greater than 24 hours when assessment.id==1
    mutate(with.timing.conflict = replace(with.timing.conflict, assessment.id==1 & interval.duration.hours>24 & smoking.label=="UNKNOWN", NA_real_))
  
  #############################################################################
  # What happens to min.time and max.time when with.timing.conflict == 1 
  #############################################################################
  
  df$max.time <- if_else(df$with.timing.conflict==1, 24*60*60, df$max.time)
  
  df$min.time <- if_else(df$min.time > df$max.time, NA_real_, df$min.time)
  df$max.time <- if_else(df$min.time > df$max.time, NA_real_, df$max.time)
  
  #############################################################################
  # Housekeeping
  #############################################################################
  
  df$max.time <- replace(df$max.time, df$smoking.label == "UNKNOWN" & df$assessment.id>1, NA_real_)
  df$min.time <- replace(df$min.time, df$smoking.label == "UNKNOWN" & df$assessment.id>1, NA_real_)
  
  df$max.time <- replace(df$max.time, df$smoking.label == "UNKNOWN" & df$assessment.id==1 & df$interval.duration.hours>24, NA_real_)
  df$min.time <- replace(df$min.time, df$smoking.label == "UNKNOWN" & df$assessment.id==1 & df$interval.duration.hours>24, NA_real_)
  
  
  return(df)
}

CreateLastTimeInterval <- function(df){
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  
  # Check: inputs are of the correct type
  assert_that(is.data.frame(df), msg = "df IS NOT A DATA FRAME")
  assert_that("interval.id" %in% colnames(df), msg = "df MUST HAVE COLUMN interval.id")
  assert_that("LB.seconds" %in% colnames(df), msg = "df MUST HAVE COLUMN LB.seconds")
  assert_that("UB.seconds" %in% colnames(df), msg = "df MUST HAVE COLUMN UB.seconds")
  assert_that("start.clock" %in% colnames(df), msg = "df MUST HAVE COLUMN start.clock")
  assert_that("end.clock" %in% colnames(df), msg = "df MUST HAVE COLUMN end.clock")
  assert_that("smoking.label" %in% colnames(df), msg = "df MUST HAVE COLUMN smoking.label")
  assert_that("num.cigs.smoked" %in% colnames(df), msg = "df MUST HAVE COLUMN num.cigs.smoked")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  ids <- unique(df$id)
  n.participants <- length(ids)
  
  list.all <- list()
  
  for(i in 1:n.participants){
    
    subset.df <- df %>% filter(id==ids[i])
    last.row <- subset.df[nrow(subset.df),]
    last.row <- as.data.frame(last.row)
    last.row["interval.id"] <- last.row["interval.id"] + 1
    last.row["LB.seconds"] <- last.row["UB.seconds"] 
    last.row["UB.seconds"] <- last.row["end.clock"] - last.row["start.clock"]
    last.row["smoking.label"] <- "UNKNOWN"
    last.row["num.cigs.smoked"] <- NA
    
    subset.df <- rbind(subset.df, last.row)
    list.all <- append(list.all, list(subset.df))
  }
  
  df <- bind_rows(list.all)
  
  return(df)
}

IntegratePuffmarkerEpisodes <- function(df.ema, df.puffmarker){
  
  ids <- unique(df.ema$id)
  n.participants <- length(ids)
  tot.intervals <- max(df.ema$interval.id)
  list.all <- list()
  
  for(i in 1:n.participants){
    
    subset.df.ema <- df.ema %>% filter(id == ids[i])
    subset.df.puffmarker <- df.puffmarker %>% filter(id == ids[i])
    
    if(nrow(subset.df.puffmarker)==0){
      list.all <- append(list.all, list(subset.df.ema))
      next
    }
    
    subset.df.puffmarker$interval.id <- apply(subset.df.puffmarker, 1, function(this.row){
      idx <- which(this.row["puffmarker.episode.secs"] > subset.df.ema[,"LB.seconds"] & this.row["puffmarker.episode.secs"] < subset.df.ema[,"UB.seconds"])
      return(subset.df.ema[idx, "interval.id"])
    })
    
    intervals.with.puffmarker <- subset.df.ema %>% filter(interval.id %in% unique(subset.df.puffmarker$interval.id))
    intervals.without.puffmarker <- subset.df.ema %>% filter(!(interval.id %in% unique(subset.df.puffmarker$interval.id)))
    
    tmp.list <- list()
    
    for(j in 1:nrow(intervals.with.puffmarker)){
      
      df.this.interval <- intervals.with.puffmarker[j,] %>% data.frame(.)
      puffmarker.this.interval <- subset.df.puffmarker %>% filter(interval.id == df.this.interval[1,"interval.id"]) %>% data.frame(.)
      addthis <- as.numeric(nrow(puffmarker.this.interval))
      current.cigs.smoked <- df.this.interval[, "num.cigs.smoked"]
      
      df.this.interval[, "num.cigs.smoked"] <- if_else(df.this.interval[, "smoking.label"] == "UNKNOWN", addthis, current.cigs.smoked + addthis)
      df.this.interval[, "smoking.label"] <- "YES"
      tmp.list <- append(tmp.list, list(df.this.interval))
      
    } # End for loop: have gone through all intervals with puffmarker timestamps
    
    tmp.df <- bind_rows(tmp.list)
    out.df <- rbind(tmp.df, intervals.without.puffmarker)
    out.df <- out.df %>% as.data.frame(.) %>% arrange(id, LB.seconds) %>% mutate(interval.id = 1:nrow(out.df))
    
    list.all <- append(list.all, list(out.df))
    
  } # End for loop: have looped through all participant IDs
  
  df.all <- bind_rows(list.all)
  
  return(df.all)
}


