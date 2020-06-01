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

###############################################################################
# Checks on number of rows excluded using the SetUpPostQuit and 
# SetUpPreQuit functions
###############################################################################
postquit.files <- c("Post_Quit_Random.csv",
                    "Post_Quit_Urge.csv",
                    "Post_Quit_About_to_Slip.csv",
                    "Post_Quit_About_to_Slip_Part2.csv",
                    "Post_Quit_Already_Slipped.csv")

postquit.colnames <- c("random","urge","abouttoslippartone","abouttoslipparttwo","alreadyslipped")

list.summarise.postquit <- list()

for(i in 1:length(postquit.files)){
  ema.item.names <- read.csv(file.path(path.pns.output_data, paste("item_names_postquit_", postquit.colnames[i],".csv", sep="")), header = TRUE, stringsAsFactors = FALSE)
  df.raw <- read.csv(file.path(path.pns.input_data, postquit.files[i]), header = TRUE, stringsAsFactors = FALSE)
  df.raw <- CheckAnyResponse(df = df.raw, keep.cols = ema.item.names[["name.codebook"]])
  df.out <- SetUpPostQuit(df.raw = df.raw)
  df.tabulate.raw <- df.raw %>% group_by(with.any.response) %>% summarise(attempted = n(), cancelled.raw = sum(Record_Status=="CANCELLED"))
  df.tabulate.out <- df.out %>% group_by(with.any.response) %>% summarise(success = n(), cancelled.out = sum(record.status=="CANCELLED"))
  df.tabulate <- cbind(df.tabulate.raw, df.tabulate.out[,2:3])
  df.tabulate[["percent.success"]] <- df.tabulate[["success"]]/df.tabulate[["attempted"]]
  df.tabulate[["percent.success"]] <- round(100*df.tabulate[["percent.success"]], digits=1)
  df.tabulate[["assessment.type"]] <- postquit.colnames[i]
  df.tabulate <- df.tabulate %>% select(assessment.type, everything())
  list.summarise.postquit <- append(list.summarise.postquit, list(df.tabulate))
}

df.summarise.postquit <- bind_rows(list.summarise.postquit)

prequit.files <- c("Pre_Quit_Random.csv",
                   "Pre_Quit_Urge.csv",
                   "Pre_Quit_Smoking.csv",
                   "Pre_Quit_Smoking_Part2.csv")

prequit.colnames <- c("random","urge","smokingpartone","smokingparttwo")

list.summarise.prequit <- list()

for(i in 1:length(prequit.files)){
  ema.item.names <- read.csv(file.path(path.pns.output_data, paste("item_names_prequit_", prequit.colnames[i],".csv", sep="")), header = TRUE, stringsAsFactors = FALSE)
  df.raw <- read.csv(file.path(path.pns.input_data, prequit.files[i]), header = TRUE, stringsAsFactors = FALSE)
  df.raw <- CheckAnyResponse(df = df.raw, keep.cols = ema.item.names[["name.codebook"]])
  df.out <- SetUpPreQuit(df.raw = df.raw)
  df.tabulate.raw <- df.raw %>% group_by(with.any.response) %>% summarise(attempted = n(), cancelled = sum(Record_Status=="CANCELLED"))
  df.tabulate.out <- df.out %>% group_by(with.any.response) %>% summarise(success = n(), cancelled = sum(record.status=="CANCELLED"))
  df.tabulate <- cbind(df.tabulate.raw, df.tabulate.out[,2])
  df.tabulate[["percent.success"]] <- df.tabulate[["success"]]/df.tabulate[["attempted"]]
  df.tabulate[["percent.success"]] <- round(100*df.tabulate[["percent.success"]], digits=1)
  df.tabulate[["assessment.type"]] <- prequit.colnames[i]
  df.tabulate <- df.tabulate %>% select(assessment.type, everything())
  list.summarise.prequit <- append(list.summarise.prequit, list(df.tabulate))
}

df.summarise.prequit <- bind_rows(list.summarise.prequit)

write.csv(df.summarise.postquit, file.path(path.pns.output_data, "summarise_postquit.csv"))
write.csv(df.summarise.prequit, file.path(path.pns.output_data, "summarise_prequit.csv"))
