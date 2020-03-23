###############################################################################
# ABOUT:
# * Creates a dataset with smoking as outcome
# * All types of EMAs are used
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.pns.code, "pns-data-manip-utils.R"))
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

#------------------------------------------------------------------------------
# Read in raw data
#------------------------------------------------------------------------------
pns.quit.dates <- read.csv(file.path(path.pns.input_data, "pns_quit_dates.csv"))
df.raw.post.quit.random <- read.csv(file.path(path.pns.input_data, "Post_Quit_Random.csv"))
df.raw.post.quit.urge <- read.csv(file.path(path.pns.input_data, "Post_Quit_Urge.csv"))
df.raw.post.quit.about.to.slip.part1 <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip.csv"))
df.raw.post.quit.about.to.slip.part2 <- read.csv(file.path(path.pns.input_data, "Post_Quit_About_to_Slip_Part2.csv"))
df.raw.post.quit.already.slipped <- read.csv(file.path(path.pns.input_data, "Post_Quit_Already_Slipped.csv"))

#------------------------------------------------------------------------------
# Tasks common to all analyses using PNS study data
#------------------------------------------------------------------------------
# Implement decision rules on constructing time frame
df.time.frame <- SetTimeFrame(df.quit.dates = pns.quit.dates, 
                              study.duration = 21, 
                              addtime = 0)

# Implement decision rules to include only "valid EMAs"
df.post.quit.random <- SetUpPostQuit(df.raw = df.raw.post.quit.random, 
                                     df.time.frame = df.time.frame)

df.post.quit.urge <- SetUpPostQuit(df.raw = df.raw.post.quit.urge, 
                                   df.time.frame = df.time.frame)

df.post.quit.about.to.slip.part1 <- SetUpPostQuit(df.raw = df.raw.post.quit.about.to.slip.part1, 
                                                  df.time.frame = df.time.frame)

df.post.quit.about.to.slip.part2 <- SetUpPostQuit(df.raw = df.raw.post.quit.about.to.slip.part2, 
                                                  df.time.frame = df.time.frame)

df.post.quit.already.slipped <- SetUpPostQuit(df.raw = df.raw.post.quit.already.slipped, 
                                              df.time.frame = df.time.frame)

#------------------------------------------------------------------------------
# Tasks specific to this dataset for analysis
#------------------------------------------------------------------------------
# Create a new variable so that for each row in the raw data 
# is checked: this new variable with.any.response
# equals 1 if response to any question was recorded
# equals 0 if no response to any question was recorded
these.cols <- c("record.id",             
                "record.status",         
                "id",                    
                "Day",                   
                "delivered.hrts",        
                "InitiatedDate",         
                "InitiatedTime",         
                "SnzCount",              
                "SnzTime1",              
                "SnzTime2",              
                "SnzTime3",              
                "SnzTime4",              
                "assessment.hrts",       
                "AssessmentCancelled",   
                "ParticipantComplied",   
                "AssessmentID",          
                "assessment.type",       
                "Asse_Type",             
                "Asse_Mode",             
                "AssessmentEnd",         
                "responded",             
                "completed",             
                "AssessmentCompleted",   
                "AssessmentNOTCompleted",
                "AssessmentTimedOut",    
                "StartDate",             
                "Quit_Date1",            
                "Quit_Date2",            
                "Quit_Date3",            
                "Quit_Date4",            
                "Quit_Date5",   
                "X",                     
                "delivered.unixts",      
                "assessment.unixts",     
                "delay",                 
                "quit.date",             
                "start.clock",           
                "end.clock") 

# Do this for all types of EMA
df.post.quit.random <- CheckAnyResponse(df = df.post.quit.random, drop.cols = these.cols)
df.post.quit.urge <- CheckAnyResponse(df = df.post.quit.urge, drop.cols = these.cols)
df.post.quit.about.to.slip.part1 <- CheckAnyResponse(df = df.post.quit.about.to.slip.part1, drop.cols = these.cols)
df.post.quit.about.to.slip.part2 <- CheckAnyResponse(df = df.post.quit.about.to.slip.part2, drop.cols = these.cols)
df.post.quit.already.slipped <- CheckAnyResponse(df = df.post.quit.already.slipped, drop.cols = these.cols)

# Create a list to contain data frames we will loop over
# to apply the same set of data preparation tasks
tmp.list.df <- list(df.post.quit.random = df.post.quit.random,
                    df.post.quit.urge = df.post.quit.urge,
                    df.post.quit.about.to.slip.part1 = df.post.quit.about.to.slip.part1,
                    df.post.quit.about.to.slip.part2 = df.post.quit.about.to.slip.part2,
                    df.post.quit.already.slipped = df.post.quit.already.slipped)

for(i in 1:length(tmp.list.df)){
  tmp.df <- tmp.list.df[[i]]
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Implement decision rules to create the variable engaged.yes
  tmp.df <- tmp.df %>% 
    mutate(engaged.yes = with.any.response)
  
  # Implement decision rules for:
  # (1) Timestamp when engaged.yes=1 and engaged.yes=0
  # (2) Timestamp when engaged.yes=1 and missing assessment.unixts
  tmp.df <- tmp.df %>%
    mutate(time.unixts = if_else(engaged.yes == 1 & !is.na(assessment.unixts), 
                                 assessment.unixts, 
                                 delivered.unixts))
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Save new info
  tmp.list.df[[i]] <- tmp.df
}

df.post.quit.random <- tmp.list.df$df.post.quit.random
df.post.quit.urge <- tmp.list.df$df.post.quit.urge
df.post.quit.about.to.slip.part1 <- tmp.list.df$df.post.quit.about.to.slip.part1
df.post.quit.about.to.slip.part2 <- tmp.list.df$df.post.quit.about.to.slip.part2
df.post.quit.already.slipped <- tmp.list.df$df.post.quit.already.slipped

remove(tmp.list.df, tmp.df)

#------------------------------------------------------------------------------
# Identify smoking items across different types of EMAs
# and create common variable names across different types of EMAs
#------------------------------------------------------------------------------
df.post.quit.random <- df.post.quit.random %>% 
  rename(smoking.indicator = PostQRSmoking1, 
         smoking.qty = Smoking2_PostQ_Random, 
         smoking.timing = Smoking3)

df.post.quit.urge <- df.post.quit.urge %>% 
  rename(smoking.indicator = SmPostQU1, 
         smoking.qty = Smoking2_PostQ_Urge, 
         smoking.timing = Smoking3)

df.post.quit.about.to.slip.part1 <- df.post.quit.about.to.slip.part1 %>% 
  mutate(smoking.indicator = NA, 
         smoking.qty = NA, 
         smoking.timing = NA)

df.post.quit.about.to.slip.part2 <- df.post.quit.about.to.slip.part2 %>% 
  rename(smoking.qty = CigJustNow_PostQ_Slip2) %>%
  mutate(smoking.indicator = NA, 
         smoking.timing = NA)

df.post.quit.already.slipped <- df.post.quit.already.slipped %>% 
  rename(smoking.qty = HowManyCig, 
         smoking.timing = LastCig) %>%
  mutate(smoking.indicator = NA)

#------------------------------------------------------------------------------
# Preparatory steps to create smoking outcome variable:
# Obtain subset of columns from the original set of EMA items
# and exclude all rows with engaged.yes==0
#------------------------------------------------------------------------------
# Create a list to contain data frames we will loop over
# to apply the same set of data preparation tasks
tmp.list.df <- list(df.post.quit.random = df.post.quit.random,
                    df.post.quit.urge = df.post.quit.urge,
                    df.post.quit.about.to.slip.part1 = df.post.quit.about.to.slip.part1,
                    df.post.quit.about.to.slip.part2 = df.post.quit.about.to.slip.part2,
                    df.post.quit.already.slipped = df.post.quit.already.slipped)

for(i in 1:length(tmp.list.df)){
  tmp.df <- tmp.list.df[[i]]
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Only retain EMAs with engaged.yes==1
  tmp.df <- tmp.df %>% filter(engaged.yes==1)
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Save new info
  tmp.list.df[[i]] <- tmp.df
}

df.post.quit.random <- tmp.list.df$df.post.quit.random
df.post.quit.urge <- tmp.list.df$df.post.quit.urge
df.post.quit.about.to.slip.part1 <- tmp.list.df$df.post.quit.about.to.slip.part1
df.post.quit.about.to.slip.part2 <- tmp.list.df$df.post.quit.about.to.slip.part2
df.post.quit.already.slipped <- tmp.list.df$df.post.quit.already.slipped

remove(tmp.list.df, tmp.df)

#------------------------------------------------------------------------------
# Select columns to retain
#------------------------------------------------------------------------------
# Column names with reference information
cols.ref <- c("id","record.id","assessment.type",
              "start.clock","end.clock",
              "time.unixts")

# Column names with smoking information
cols.items.smoking <- c("smoking.indicator","smoking.qty","smoking.timing")

# Create a list to contain data frames we will loop over
# to apply the same set of data preparation tasks
tmp.list.df <- list(df.post.quit.random = df.post.quit.random,
                    df.post.quit.urge = df.post.quit.urge,
                    df.post.quit.about.to.slip.part1 = df.post.quit.about.to.slip.part1,
                    df.post.quit.about.to.slip.part2 = df.post.quit.about.to.slip.part2,
                    df.post.quit.already.slipped = df.post.quit.already.slipped)

for(i in 1:length(tmp.list.df)){
  tmp.df <- tmp.list.df[[i]]
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  tmp.df <- tmp.df %>% select(c(cols.ref, cols.items.smoking))
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # Save new info
  tmp.list.df[[i]] <- tmp.df
}

outcomedf.post.quit.random <- tmp.list.df$df.post.quit.random
outcomedf.post.quit.urge <- tmp.list.df$df.post.quit.urge
outcomedf.post.quit.about.to.slip.part1 <- tmp.list.df$df.post.quit.about.to.slip.part1
outcomedf.post.quit.about.to.slip.part2 <- tmp.list.df$df.post.quit.about.to.slip.part2
outcomedf.post.quit.already.slipped <- tmp.list.df$df.post.quit.already.slipped

remove(tmp.list.df, tmp.df)

#------------------------------------------------------------------------------
# Merge all data from different types of post quit EMA into one data frame
#------------------------------------------------------------------------------
outcomedf.all <- rbind(outcomedf.post.quit.random,
                       outcomedf.post.quit.urge,
                       outcomedf.post.quit.about.to.slip.part1,
                       outcomedf.post.quit.about.to.slip.part2,
                       outcomedf.post.quit.already.slipped)

# #############################################################################
# Perform sanity checks on data at this point: change do.checks to TRUE
# #############################################################################
do.checks <-TRUE

if(isTRUE(do.checks)){
  # Count number of EMAs with missing values in the
  # smoking.indicator, smoking.qty, smoking.timing variables
  outcomedf.all %>% 
    mutate(miss1 = 1*is.na(smoking.indicator),
           miss2 = 1*is.na(smoking.qty),
           miss3 = 1*is.na(smoking.timing)) %>%
    mutate(num.missing =  miss1 + miss2 + miss3) %>%
    select(num.missing) %>% table(.)
  
  # Check whether there are missing timestamps
  # and print out rows in dataset corresponding to these observations, if any
  if(sum(is.na(outcomedf.all$time.unixts))>0){
    print(outcomedf.all[is.na(outcomedf.all$time.unixts),])
  }
}

#------------------------------------------------------------------------------
# More preparatory steps to create smoking outcome variable:
# Create end points of time inetrval
#------------------------------------------------------------------------------
outcomedf.all <- outcomedf.all %>% 
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., 
                    cols.today = c("assessment.type", "time.unixts"), 
                    h = c(1,1),
                    this.numeric = c(FALSE, TRUE))
  )

# Count number of EMAs per participant in outcomedf.all
outcomedf.all  <- outcomedf.all %>%
  arrange(id, time.unixts) %>%
  group_by(id) %>%
  do(.data = ., mutate(.data = ., ema.id = 1:nrow(.)))

# Identify which column names corresopnd to lower bound and upper bound of time intervals
outcomedf.all <- outcomedf.all %>% 
  mutate(LB.ts = time.unixts_shift.minus.1, UB.ts = time.unixts,
         LB.type = assessment.type_shift.minus.1, UB.type = assessment.type)

# Create new time-related variables
outcomedf.all <- outcomedf.all %>% 
  # Since LB.ts and UB.ts are in seconds elapsed ince start.clock
  mutate(LB.ts = if_else(ema.id == 1, start.clock, LB.ts)) %>%
  mutate(interval.duration.secs = UB.ts - LB.ts) %>%
  mutate(interval.duration.hours = interval.duration.secs/(60*60))

#------------------------------------------------------------------------------
# More preparatory steps to create smoking outcome variable:
# Clean up raw responses to EMA smoking items
#------------------------------------------------------------------------------
outcomedf.all <- outcomedf.all %>% 
  group_by(UB.type) %>% 
  do(PNSCleanSmokingCount(df = .))

outcomedf.all <- outcomedf.all %>% 
  arrange(id, time.unixts) %>% 
  group_by(id) %>% 
  do(PNSCleanSmokingTime(df = .))

# For each individual, rearrange rows chronologically
outcomedf.all <- outcomedf.all %>% arrange(id, time.unixts) %>% 
  select(id, record.id, ema.id, time.unixts,
         start.clock, end.clock,
         LB.ts, UB.ts,
         LB.type, UB.type,
         interval.duration.secs,
         interval.duration.hours,
         max.time, min.time,
         num.cigs.smoked,
         smoking.label,
         with.timing.conflict)

#------------------------------------------------------------------------------
# Version 01: no refinement of end points based on reported timing of
# smoking event
#------------------------------------------------------------------------------
df.smoking.01 <- outcomedf.all %>%
  select(id, interval.id = ema.id, 
         start.clock, end.clock, LB.ts, UB.ts,
         smoking.label, num.cigs.smoked)

# Now we have cleaned smoking outcome data
write.csv(df.smoking.01, 
          file.path(path.pns.output_data, "pns.smoking.01.csv"), 
          row.names = FALSE)

#------------------------------------------------------------------------------
# Version 02: Refine end points based on reported timing of smoking event
#------------------------------------------------------------------------------
# More preparatory steps to create smoking outcome variable
df.smoking.02 <- outcomedf.all %>% 
  arrange(id, time.unixts) %>% 
  group_by(id) %>% 
  do(PNSRefineSmokingTime(df = .))

# Now we have cleaned smoking outcome data
write.csv(df.smoking.02, 
          file.path(path.pns.output_data, "pns.smoking.02.csv"), 
          row.names = FALSE)

#------------------------------------------------------------------------------
# For each row in use.df.smoking, search for the record id
# of the most proximal Random EMA in the past and the most proximal
# Random EMA in the future
#
# use.df.smoking can be set to any of the smoking outcome data frames above,
# such as df.smoking.01, df.smoking.02
#------------------------------------------------------------------------------
this.file <- "pns.smoking.02"  # This can be changed to another file

# Read file
use.df.smoking <- read.csv(file.path(path.pns.output_data, paste(this.file, ".csv", sep="")))
use.name <- substring(this.file, first = 13)

# Convert use.df.smoking to a data frame with two columns
# First column: each row is an individual's id
# Second column: each row is a data frame containing rows in use.df.smoking
# of the individual
reshaped.use.df.smoking <- use.df.smoking %>% group_by(id) %>% do(outcome = data.frame(.))

###############################################################################
# Read in curated Random EMA data; this file.name may be changed to alternate
# versions, e.g. pns.postquitrandom.XX.csv
###############################################################################
this.file.name <- "pns.postquitrandom.01.csv"

# Continue --------------------------------------------------------------------
use.df.covariates <- read.csv(file.path(path.pns.output_data, this.file.name), header = TRUE)

# Convert use.df.covariates to a data frame with two columns
# First column: each row is an individual's id
# Second column: each row is a data frame containing rows in use.df.covariates
# of the individual
reshaped.use.df.covariates <- use.df.covariates %>% group_by(id) %>% do(covariates = data.frame(.))

# Merge smoking outcome data frame and curated random EMA data 
merged.df <- left_join(x = reshaped.use.df.smoking, y = reshaped.use.df.covariates, by = "id")

# Now, loop through all individuals
ids <- unique(merged.df$id)
n <- length(ids)

list.reference.df <- list()

for(i in 1:n){
  this.id <- ids[i]
  this.df.outcome <- merged.df %>% 
    filter(id == this.id) %>% 
    select(outcome) %>% 
    extract2(1) %>% extract2(1)
  
  this.df.covariate <- merged.df %>% 
    filter(id == this.id) %>% 
    select(covariates) %>%
    extract2(1) %>% extract2(1)
  
  this.df.outcome$proximal.past.record.id <- NA
  this.df.outcome$proximal.future.record.id <- NA
  
  for(j in 1:nrow(this.df.outcome)){
    # Get record.id from most proximal random EMA prior to LB.ts
    use.ts <- this.df.outcome[j,]$LB.ts
    this.record.id <- SearchRecordID(timestamp = use.ts, 
                                     df.covariate = this.df.covariate, 
                                     past = TRUE)
    this.df.outcome[j,]$proximal.past.record.id <- this.record.id
    
    # Get record.id from most proximal random EMA after UB.ts
    use.ts <- this.df.outcome[j,]$UB.ts
    this.record.id <- SearchRecordID(timestamp = use.ts, 
                                     df.covariate = this.df.covariate, 
                                     past = FALSE)
    this.df.outcome[j,]$proximal.future.record.id <- this.record.id
  }
  
  list.reference.df <- append(list.reference.df, list(this.df.outcome))
}

reference.df <- bind_rows(list.reference.df)

# Now we have cleaned smoking outcome data with a way to grab items from random EMAs
write.csv(reference.df, 
          file.path(path.pns.output_data, 
                    paste("pns", "reference", use.name,"csv", sep=".")), 
          row.names = FALSE)

#------------------------------------------------------------------------------
# Using reference.df, append responses to Random EMAs in use.df.covariates
#------------------------------------------------------------------------------
drop.these.cols <- c("id", "record.id", 
                     "start.clock", "end.clock",
                     "time.unixts", "time.unixts.scaled", 
                     "engaged.yes",
                     "total.prompts.since.start")

get.these.cols <- setdiff(colnames(use.df.covariates), drop.these.cols)
new.cols.proximal.past <- paste(get.these.cols, "proximal.past", sep="_")
new.cols.proximal.future <- paste(get.these.cols, "proximal.future", sep="_")

# This will contain reference.df and new columns corresponding to
# individuals' responses to Random EMA items for record id's 
# in each row of reference.df
list.reference.df.appended <- list()

# Loop through all rows of reference.df
for(i in 1:nrow(reference.df)){
  # Work with row i
  this.row <- reference.df[i,]
  past.record.id <- this.row[["proximal.past.record.id"]]
  future.record.id <- this.row[["proximal.future.record.id"]]
  
  # Get responses corresponding to most proximal random EMA
  # in the past
  if(!is.na(past.record.id)){
    grabbed.data <- use.df.covariates %>% 
      filter(record.id == past.record.id) %>% 
      select(get.these.cols) %>%
      set_colnames(new.cols.proximal.past)
    
    this.row[new.cols.proximal.past] <- grabbed.data
  }else{
    this.row[new.cols.proximal.past] <- NA
  }
  # Get responses corresponding to most proximal random EMA
  # in the future
  if(!is.na(future.record.id)){
    grabbed.data <- use.df.covariates %>% 
      filter(record.id == future.record.id) %>% 
      select(get.these.cols) %>%
      set_colnames(new.cols.proximal.future)
    
    this.row[new.cols.proximal.future] <- grabbed.data
  }else{
    this.row[new.cols.proximal.future] <- NA
  }
  
  list.reference.df.appended <- append(list.reference.df.appended, 
                                       list(this.row))
}

reference.df.appended <- bind_rows(list.reference.df.appended)

write.csv(reference.df.appended, 
          file.path(path.pns.output_data, 
                    paste("pns", "reference", use.name,"appended.csv", sep=".")), 
          row.names = FALSE)

