###############################################################################
# ABOUT:
# * Creates a dataset with time when an Urge EMA was triggered as outcome
# * Only Urge EMAs are used in this script
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
df.raw.post.quit.urge <- read.csv(file.path(path.pns.input_data, "Post_Quit_Urge.csv"))

#------------------------------------------------------------------------------
# Tasks common to all analyses using PNS study data
#------------------------------------------------------------------------------
# Implement decision rules on constructing time frame
df.time.frame <- SetTimeFrame(df.quit.dates = pns.quit.dates, 
                              study.duration = 21, 
                              addtime = 0)
# Implement decision rules to include only "valid EMAs"
df.post.quit.urge <- SetUpPostQuit(df.raw = df.raw.post.quit.urge, 
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

df.post.quit.urge <- CheckAnyResponse(df = df.post.quit.urge, drop.cols = these.cols)

# Implement decision rules to create the variable engaged.yes
df.post.quit.urge <- df.post.quit.urge %>% 
  mutate(engaged.yes = with.any.response) %>%
  filter(engaged.yes == 1)

# Implement decision rules for:
# (1) Timestamp when engaged.yes=1
df.post.quit.urge <- df.post.quit.urge %>%
  mutate(time.unixts = if_else(engaged.yes == 1 & !is.na(assessment.unixts), 
                               assessment.unixts, 
                               delivered.unixts)) %>%
  mutate(time.unixts.scaled = time.unixts - start.clock) %>%
  mutate(delivered.unixts.scaled = delivered.unixts - start.clock) %>%
  mutate(assessment.unixts.scaled = assessment.unixts - start.clock)

#------------------------------------------------------------------------------
# Create new variables involving counts of Urge EMAs
#------------------------------------------------------------------------------
df.post.quit.urge <- df.post.quit.urge %>% 
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
  assert_that(anyDuplicated(df.post.quit.urge[["record.id"]])==0,
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
                         "SSPostQU1","SocialSet2","SSPostQU3", "SocialSet4",
                         "SocialSet4","Restriction")

# Column names of questions framed in terms of the past time
cols.items.past <- c("Stressor1_PostQ_Urge","Stressor2",
                     "Lonely1","Lonely2","Lonely3",
                     "D1PostQU",
                     "Distract1","Distract2","Distract3","Distract4",
                     "Consume1","C2PostQU")

df.out.01 <- df.post.quit.urge %>% select(c(cols.ref, cols.items.now))
df.out.02 <- df.post.quit.urge %>% select(c(cols.ref, cols.items.now, cols.items.now.more))
df.out.03 <- df.post.quit.urge %>% select(c(cols.ref, cols.items.now, cols.items.now.more, cols.items.past))

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------
write.csv(df.out.01, 
          file.path(path.pns.output_data, "pns.postquiturge.01.csv"), 
          row.names=FALSE)

write.csv(df.out.02, 
          file.path(path.pns.output_data, "pns.postquiturge.02.csv"), 
          row.names=FALSE)

write.csv(df.out.03, 
          file.path(path.pns.output_data, "pns.postquiturge.03.csv"), 
          row.names=FALSE)

