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
                                 delivered.unixts)) %>%
    mutate(time.unixts.scaled = time.unixts - start.clock) %>%
    mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
    mutate(assessment.unixts.scaled = assessment.unixts - start.clock)
  
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
              "time.unixts","time.unixts.scaled")

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
  arrange(id, time.unixts.scaled) %>%
  group_by(id) %>%
  do(GetPastRecords(df.this.group = ., 
                    cols.today = c("assessment.type", "time.unixts.scaled"), 
                    h = c(1,1),
                    this.numeric = c(FALSE, TRUE))
  )

# Count number of EMAs per participant in outcomedf.all
outcomedf.all  <- outcomedf.all %>%
  arrange(id, time.unixts.scaled) %>%
  group_by(id) %>%
  do(.data = ., mutate(.data = ., ema.id = 1:nrow(.)))

# Identify which column names corresopnd to lower bound and upper bound of time intervals
outcomedf.all <- outcomedf.all %>% 
  mutate(LB.ts = time.unixts.scaled_shift.minus.1, UB.ts = time.unixts.scaled,
         LB.type = assessment.type_shift.minus.1, UB.type = assessment.type)

# Create new time-related variables
outcomedf.all <- outcomedf.all %>% 
  # Since LB.ts and UB.ts are in seconds elapsed ince start.clock
  mutate(LB.ts = if_else(ema.id == 1, 0, LB.ts)) %>%
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
  select(id, record.id, ema.id,
         start.clock, end.clock,
         LB.ts, UB.ts,
         LB.type, UB.type,
         interval.duration.hours,
         max.time, min.time,
         num.cigs.smoked,
         smoking.label,
         everything())

