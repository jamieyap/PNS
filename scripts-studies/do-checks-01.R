#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)
library(ggplot2)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.shared.code, "shared-data-manip-utils.R"))
source(file.path(path.pns.code, "data-manip-utils.R"))

ema.item.names <- read.csv(file.path(path.pns.output_data, "ema_item_names.csv"), header = TRUE, stringsAsFactors = FALSE)

###############################################################################
# Checks on Post-Quit EMAs
###############################################################################
postquit.files <- c("Post_Quit_Random.csv",
                    "Post_Quit_Urge.csv",
                    "Post_Quit_About_to_Slip.csv",
                    "Post_Quit_About_to_Slip_Part2.csv",
                    "Post_Quit_Already_Slipped.csv")

postquit.colnames <- c("Post-Quit Random",
                       "Post-Quit Urge",
                       "Post-Quit About to Slip Part One",
                       "Post-Quit About to Slip Part Two",
                       "Post-Quit Already Slipped")

list.summarise.postquit <- list()

for(i in 1:length(postquit.files)){
  df.raw <- read.csv(file.path(path.pns.input_data, postquit.files[i]), header = TRUE, stringsAsFactors = FALSE)
  df.raw <- CheckAnyResponse(df = df.raw, keep.cols = (ema.item.names %>% filter(assessment.type==postquit.colnames[i]) %>% extract2("name.codebook")))
  df.out <- CreateEMATimeVars(df.raw = df.raw) %>% filter(is.delivered==1)
  df.tabulate.raw <- df.raw %>% 
    group_by(with.any.response) %>% 
    summarise(attempted = n(), 
              completed.raw = sum(Record_Status=="Completed"),
              cancelled.raw = sum(Record_Status=="CANCELLED"), 
              timedout.raw = sum(Record_Status=="Incomplete/Timed Out"), 
              fragment.raw = sum(Record_Status=="FRAGMENT RECORD"))
  df.tabulate.out <- df.out %>% 
    group_by(with.any.response) %>% 
    summarise(success = n(),
              completed.out = sum(record.status=="Completed"),
              cancelled.out = sum(record.status=="CANCELLED"), 
              timedout.out = sum(record.status=="Incomplete/Timed Out"), 
              fragment.out = sum(record.status=="FRAGMENT RECORD"))
  df.tabulate <- cbind(df.tabulate.raw, df.tabulate.out[,2:6])
  df.tabulate[["percent.success"]] <- df.tabulate[["success"]]/df.tabulate[["attempted"]]
  df.tabulate[["percent.success"]] <- round(100*df.tabulate[["percent.success"]], digits=1)
  df.tabulate[["assessment.type"]] <- postquit.colnames[i]
  df.tabulate <- df.tabulate %>% select(assessment.type, everything())
  list.summarise.postquit <- append(list.summarise.postquit, list(df.tabulate))
}

df.summarise.postquit <- bind_rows(list.summarise.postquit)

###############################################################################
# Checks on Pre-Quit EMAs
###############################################################################

prequit.files <- c("Pre_Quit_Random.csv",
                   "Pre_Quit_Urge.csv",
                   "Pre_Quit_Smoking.csv",
                   "Pre_Quit_Smoking_Part2.csv")

prequit.colnames <- c("Pre-Quit Random",
                      "Pre-Quit Urge",
                      "Pre-Quit Smoking Part One",
                      "Pre-Quit Smoking Part Two")

list.summarise.prequit <- list()

for(i in 1:length(prequit.files)){
  df.raw <- read.csv(file.path(path.pns.input_data, prequit.files[i]), header = TRUE, stringsAsFactors = FALSE)
  df.raw <- CheckAnyResponse(df = df.raw, keep.cols = (ema.item.names %>% filter(assessment.type==prequit.colnames[i]) %>% extract2("name.codebook")))
  df.out <- CreateEMATimeVars(df.raw = df.raw) %>% filter(is.delivered==1)
  df.tabulate.raw <- df.raw %>% 
    group_by(with.any.response) %>% 
    summarise(attempted = n(), 
              completed.raw = sum(Record_Status=="Completed"),
              cancelled.raw = sum(Record_Status=="CANCELLED"), 
              timedout.raw = sum(Record_Status=="Incomplete/Timed Out"), 
              fragment.raw = sum(Record_Status=="FRAGMENT RECORD"))
  df.tabulate.out <- df.out %>% 
    group_by(with.any.response) %>% 
    summarise(success = n(),
              completed.out = sum(record.status=="Completed"),
              cancelled.out = sum(record.status=="CANCELLED"), 
              timedout.out = sum(record.status=="Incomplete/Timed Out"), 
              fragment.out = sum(record.status=="FRAGMENT RECORD"))
  df.tabulate <- cbind(df.tabulate.raw, df.tabulate.out[,2:6])
  df.tabulate[["percent.success"]] <- df.tabulate[["success"]]/df.tabulate[["attempted"]]
  df.tabulate[["percent.success"]] <- round(100*df.tabulate[["percent.success"]], digits=1)
  df.tabulate[["assessment.type"]] <- prequit.colnames[i]
  df.tabulate <- df.tabulate %>% select(assessment.type, everything())
  list.summarise.prequit <- append(list.summarise.prequit, list(df.tabulate))
}

df.summarise.prequit <- bind_rows(list.summarise.prequit)

###############################################################################
# Write out results to csv file
###############################################################################

df.summarise.postquit.0 <- df.summarise.postquit %>% filter(with.any.response==0) %>% select(-with.any.response)
df.summarise.postquit.1 <- df.summarise.postquit %>% filter(with.any.response==1) %>% select(-with.any.response)

write.csv(df.summarise.postquit.0, file.path(path.pns.output_data, "checks_output/summarise_postquit_1a.csv"), row.names = FALSE)
write.csv(df.summarise.postquit.1 , file.path(path.pns.output_data, "checks_output/summarise_postquit_1b.csv"), row.names = FALSE)


df.summarise.prequit.0 <- df.summarise.prequit %>% filter(with.any.response==0) %>% select(-with.any.response)
df.summarise.prequit.1 <- df.summarise.prequit %>% filter(with.any.response==1) %>% select(-with.any.response)

write.csv(df.summarise.prequit.0, file.path(path.pns.output_data, "checks_output/summarise_prequit_1a.csv"), row.names = FALSE)
write.csv(df.summarise.prequit.1, file.path(path.pns.output_data, "checks_output/summarise_prequit_1b.csv"), row.names = FALSE)



