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
# Checks on number of rows excluded using the SetUpPostQuit and 
# SetUpPreQuit functions
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
  df.tabulate <- quantile((df.out$begin.unixts - df.out$delivered.unixts), c(.50, .75, .95), na.rm=TRUE)
  df.tabulate <- as.data.frame(df.tabulate)
  df.tabulate <- t(as.matrix(df.tabulate))
  df.tabulate <- cbind(postquit.colnames[i], df.tabulate)
  list.summarise.postquit <- append(list.summarise.postquit, list(df.tabulate))
}

df.summarise.postquit <- do.call(rbind,list.summarise.postquit)

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
  df.tabulate <- quantile((df.out$begin.unixts - df.out$delivered.unixts), c(.50, .75, .95), na.rm=TRUE)
  df.tabulate <- t(as.matrix(df.tabulate))
  df.tabulate <- cbind(prequit.colnames[i], df.tabulate)
  list.summarise.prequit <- append(list.summarise.prequit, list(df.tabulate))
}

df.summarise.prequit <- do.call(rbind,list.summarise.prequit)

write.csv(df.summarise.postquit, file.path(path.pns.output_data, "summarise_postquit_3.csv"), row.names = FALSE)
write.csv(df.summarise.prequit, file.path(path.pns.output_data, "summarise_prequit_3.csv"), row.names = FALSE)

