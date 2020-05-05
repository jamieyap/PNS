###############################################################################
# ABOUT:
# * Creates a dataset with EMA completion as outcome
# * Only Random EMAs are used; other types of EMAs are not used
###############################################################################

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
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
                "callnumr",
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

df.post.quit.random <- CheckAnyResponse(df = df.post.quit.random, drop.cols = these.cols)

# Implement decision rules to create the variable engaged.yes
df.post.quit.random <- df.post.quit.random %>% 
  mutate(engaged.yes = with.any.response)

# Implement decision rules for:
# (1) Timestamp when engaged.yes=1 and engaged.yes=0
df.post.quit.random <- df.post.quit.random %>%
  mutate(time.unixts = if_else(engaged.yes == 1, assessment.unixts, delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

#------------------------------------------------------------------------------
# Create new variables involving counts of Random EMAs
#------------------------------------------------------------------------------
df.post.quit.random <- df.post.quit.random %>% 
  mutate(ones=1) %>%
  group_by(id) %>% 
  # Total number of prompts from start.clock up to the present
  mutate(total.prompts.since.start=cumsum(ones)) %>%
  select(-ones)

###############################################################################
# Sanity check: record.id values do not have any duplicates
###############################################################################
check.duplicates <- TRUE

if(isTRUE(check.duplicates)){
  assert_that(anyDuplicated(df.post.quit.random[["record.id"]])==0,
              msg = "duplicate record id's")
}

#------------------------------------------------------------------------------
# Obtain subset of columns from the original set of EMA items
#------------------------------------------------------------------------------
# Column names with reference information
cols.ref <- c("id","record.id",
              "start.clock","end.clock",
              "time.unixts","time.unixts.scaled",
              "engaged.yes",
              "total.prompts.since.start")

# Column names of questions framed in terms of the present time,
# excluding smoking-related items
cols.items.now <- c("Urge1","Urge2","Urge3",
                    "Affect1","Affect2","Affect3","Affect4","Affect5",
                    "Affect6","Affect7","Affect8","Affect9","Affect10",
                    "Expect1","Expect2","AbsSelfEff",
                    "Motive1","Motive2")

# Column names of questions framed in terms of the present time,
# including only smoking-related items
cols.items.now.more <- c("CigAv",
                         "SSPostQR1","SocialSet2","SSPostQR3", "SocialSet4",
                         "SocialSet4","Restriction")

# Column names of questions framed in terms of the past time
cols.items.past <- c("Stressor1_PostQ_Random","Stressor2",
                     "Lonely1","Lonely2","Lonely3",
                     "D1PostQR",
                     "Distract1","Distract2","Distract3","Distract4",
                     "Consume1","C2PostQR")

df.out.01 <- df.post.quit.random %>% select(c(cols.ref, cols.items.now))
df.out.02 <- df.post.quit.random %>% select(c(cols.ref, cols.items.now, cols.items.now.more))
df.out.03 <- df.post.quit.random %>% select(c(cols.ref, cols.items.now, cols.items.now.more, cols.items.past))

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------
write.csv(df.out.01, 
          file.path(path.pns.output_data, "pns.postquitrandom.01.csv"), 
          row.names=FALSE)

write.csv(df.out.02, 
          file.path(path.pns.output_data, "pns.postquitrandom.02.csv"), 
          row.names=FALSE)

write.csv(df.out.03, 
          file.path(path.pns.output_data, "pns.postquitrandom.03.csv"), 
          row.names=FALSE)

#------------------------------------------------------------------------------
# Optionally construct new features
#------------------------------------------------------------------------------
construct.features <- TRUE
df.out <- df.out.01

if(isTRUE(construct.features)){
  # Features: means and variances of responses
  source(file.path(path.pns.code, "pns-features.R"))
  # Feature: variance of responses within a given EMA
  df.more.features <- df.out %>% 
    group_by(id, record.id) %>% 
    summarise(variance.among.affect = var(c(Affect1, Affect2, Affect3, Affect4, 
                                            Affect5, Affect6, Affect7, Affect8, 
                                            Affect9, Affect10),
                                          na.rm = TRUE))
  df.out <- left_join(x = df.out, y = df.more.features, by = c("id", "record.id"))
  
  # Clean up output and save
  df.out <- select(df.out, -ones)
  
  write.csv(df.out, 
            file.path(path.pns.output_data, 
                      "pns.postquitrandom.with.new.vars.csv"), 
            row.names=FALSE)
}

