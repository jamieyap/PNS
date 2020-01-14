# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the Break Free study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"ontrack-run-curation/ontrack-setup.R"))

#------------------------------------------------------------------------------
# Specify columns to use in creating data for analyses
#------------------------------------------------------------------------------

these.cols <- c("id", "delivered.assessment.id", "record.status",
                "start.clock", "end.clock",
                # Assessment type (eg. random EMA, smoking EMA, stress EMA)
                "assessment.type", 
                # Timestamp: when EMA was delivered
                "delivered.unixts",
                # Timestamp: when individual began completing EMA
                "assessment.unixts",
                # Indicator of whether there is a record of participant completing at least one question
                "with.any.response",
                # Emotion related questions and their timestamps
                "Calm.", "Attentive.", "Active.", "Tired.", "Nervous.", "Proud.",
                "Irritable.","Grateful.","Happy.","Bored.",
                "Scared.","Disgusted.","Angry.","Sad.")

df.all <- ontrack.ema %>% select(these.cols) %>% filter(assessment.type=="RandomEMA") %>%
  mutate(isCompleted = if_else(record.status=="COMPLETED",1,0)) %>%
  mutate(time.unixts = if_else(with.any.response==1, assessment.unixts, delivered.unixts)) %>%
  arrange(id, time.unixts)

df.all[,"random.assessment.id"] <- CreateID(dat = df.all, 
                                            sequence.along = "delivered.assessment.id", 
                                            by.var = "id", 
                                            id.name = "random.assessment.id")

df.all <- df.all %>% 
  mutate(Calm. = as.numeric(as.character(Calm.))) %>%
  mutate(Attentive. = as.numeric(as.character(Attentive.))) %>%
  mutate(Active. = as.numeric(as.character(Active.))) %>%
  mutate(Tired. = as.numeric(as.character(Tired.))) %>%
  mutate(Nervous. = as.numeric(as.character(Nervous.))) %>%
  mutate(Proud. = as.numeric(as.character(Proud.))) %>%
  mutate(Irritable. = as.numeric(as.character(Irritable.))) %>%
  mutate(Grateful. = as.numeric(as.character(Grateful.))) %>%
  mutate(Happy. = as.numeric(as.character(Happy.))) %>%
  mutate(Bored. = as.numeric(as.character(Bored.))) %>%
  mutate(Scared. = as.numeric(as.character(Scared.))) %>%
  mutate(Disgusted. = as.numeric(as.character(Disgusted.))) %>%
  mutate(Angry. = as.numeric(as.character(Angry.))) %>%
  mutate(Sad. = as.numeric(as.character(Sad.)))

ids <- unique(df.all$id)
n.participants <- length(ids)

tmp.list <- list()
for(i in 1:n.participants){
  df.participant <- df.all %>% filter(id == ids[i])
  df.participant <- GetPastRecords(df.this.group = df.participant, 
                                   cols.today = c("time.unixts","isCompleted","Calm.", "Attentive.", "Active.", "Tired.", "Nervous.", "Proud.",
                                                  "Irritable.","Grateful.","Happy.","Bored.",
                                                  "Scared.","Disgusted.","Angry.","Sad."), 
                                   h =c(1,1,1,1,1,1,1,1,1,1,1,1,
                                        1,1,1,1))
  tmp.list <- append(tmp.list, list(df.participant))
}

df.all <- bind_rows(tmp.list)

df.all <- df.all %>%
  mutate(time.unixts_lag1 = if_else(random.assessment.id==1,start.clock,time.unixts_lag1)) %>%
  mutate(interval.duration.secs = time.unixts - time.unixts_lag1) %>%
  mutate(interval.duration.hours = interval.duration.secs/(60*60)) %>%
  mutate(LB.seconds = time.unixts_lag1 - start.clock) %>%
  mutate(UB.seconds = time.unixts - start.clock)

df.all <- df.all %>% filter(random.assessment.id>1) %>%
  select(id, delivered.assessment.id, random.assessment.id, start.clock, end.clock, 
         with.any.response, isCompleted, isCompleted_lag1,
         LB.seconds, UB.seconds, 
         interval.duration.secs, interval.duration.hours,
         Calm., Attentive., Active., Tired., Nervous., Proud.,
         Irritable.,Grateful.,Happy.,Bored.,
         Scared.,Disgusted.,Angry.,Sad.,
         Calm._lag1, Attentive._lag1, Active._lag1, Tired._lag1, Nervous._lag1,  Proud._lag1,
         Irritable._lag1,Grateful._lag1,Happy._lag1,Bored._lag1,
         Scared._lag1, Disgusted._lag1, Angry._lag1,Sad._lag1)


write.csv(df.all, file.path(path.output_data, "ontrack/df.analysis.csv"), row.names = FALSE)



