#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.output_data <- Sys.getenv("path.pns.output_data")

#------------------------------------------------------------------------------
# Get beginning and end dates for each individual
#------------------------------------------------------------------------------
df.dates <- read.csv(file.path(path.pns.output_data, "dates.csv"), stringsAsFactors = FALSE)

df.usedates <- df.dates %>% 
  select(id, callnumr, 
         use.begin.date.value, use.end.date.value,
         use.begin.time.value, use.end.time.value) %>%
  mutate(begin.date.hrts = paste(use.begin.date.value, use.begin.time.value, sep=" "),
         end.date.hrts = paste(use.end.date.value, use.end.time.value, sep=" ")) %>%
  mutate(begin.date.hrts = if_else(begin.date.hrts=="", NA_character_, begin.date.hrts),
         end.date.hrts = if_else(end.date.hrts=="", NA_character_, end.date.hrts)) %>%
  mutate(begin.date.hrts = as.POSIXct(strptime(begin.date.hrts, format = "%Y-%m-%d %H:%M:%S")),
         end.date.hrts = as.POSIXct(strptime(end.date.hrts, format = "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(begin.date.unixts = as.numeric(begin.date.hrts),
         end.date.unixts = as.numeric(end.date.hrts)) %>%
  filter(!is.na(begin.date.unixts)) %>%
  select(-use.begin.date.value, -use.end.date.value,
         -use.begin.time.value, -use.end.time.value)

#------------------------------------------------------------------------------
# Create data frame with one participant day per row
#------------------------------------------------------------------------------
list.collect <- list()

for(i in 1:nrow(df.usedates)){
  df <- data.frame(id = df.usedates[i,"id"],
                   current.date.hrts = seq(from = df.usedates[i,"begin.date.hrts"], to = df.usedates[i,"end.date.hrts"], by = 24*60*60))
  
  df[["previous.date.hrts"]] <- c(df[1,"current.date.hrts"]-24*60*60, head(df[["current.date.hrts"]], -1))
  df[["current.date.unixts"]] <- as.numeric(df[["current.date.hrts"]])
  df[["previous.date.unixts"]] <- c(df[1,"current.date.unixts"]-24*60*60, head(df[["current.date.unixts"]], -1))
  
  list.collect <- append(list.collect, list(df))
}

#------------------------------------------------------------------------------
# Count number of cigarettes smoked per day for each individual
#------------------------------------------------------------------------------
df.smoking <- read.csv(file.path(path.pns.output_data, "smoking.csv"), stringsAsFactors = FALSE)

for(i in 1:length(list.collect)){
  current.df <- list.collect[[i]]
  current.df[["count"]] <- NA
  for(j in 1:nrow(current.df)){
    current.id <- current.df[j,"id"]
    previous.date.unixts <- current.df[j,"previous.date.unixts"]
    current.date.unixts <- current.df[j,"current.date.unixts"]
    
    df.smoking.subset <- df.smoking %>% 
      filter(id == current.id) %>%
      filter(current.time.unixts > previous.date.unixts & current.time.unixts <= current.date.unixts)
    
    if(nrow(df.smoking.subset)==0){
      count <- NA
    }else if(sum(is.na(df.smoking.subset[["smoking.qty"]])) == length(df.smoking.subset[["smoking.qty"]])){
      count <- NA
    }else{
      count <- sum(df.smoking.subset[["smoking.qty"]], na.rm=TRUE)
    }
    current.df[j,"count"] <- count
  }
  list.collect[[i]] <- current.df
}

df.collect <- bind_rows(list.collect)

#------------------------------------------------------------------------------
# Save data frame
#------------------------------------------------------------------------------
write.csv(df.collect, file.path(path.pns.output_data, "counts.csv"), row.names = FALSE, na="")


