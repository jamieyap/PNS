library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

CheckAnyResponse <- function(df, keep.cols){
  # About: Check whether there is a recorded response to each row of the 
  #   data frame df
  # Args:
  #   df: data frame containing raw data
  #   keep.cols: names of columns to keep from df when checking 
  #     whether any response was recorded in each row
  # Output:
  #   df with a new columns with.any.response: this variable is equal to 1
  #   if there is a record of at least one question completed and 0 if there
  #   is no record of any question completed
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(is.character(keep.cols), msg = "keep.cols must be a character array")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  df.items <- df %>% select(keep.cols)
  
  if(ncol(df.items)>0){
    df.items <- apply(df.items, 2, function(x){
      x <- as.character(x)
      x <- ifelse(is.na(x), NA_character_,
                  ifelse(x=="", NA_character_, x))
      return(x)
    })
    
    # nrow(df.items)==NULL if df.items contains only 1 row
    if(is.null(nrow(df.items))){
      df.items <- as.matrix(df.items)
      df.items <- t(df.items)
    }
    
    count.num.response <- df.items %>% is.na(.) %>% not(.) %>% rowSums(.)
    with.any.response <- if_else(count.num.response > 0, 1, 0)

  }else{
   with.any.response <- rep(0, times = nrow(df)) 
  }
  df <- df %>% mutate(with.any.response = with.any.response)
  
  return(df)
}


GetFutureRecords <- function(df.this.group, cols.today, h, this.numeric){
  # About: Create new variables using past records
  # 
  # Args:
  #   df.this.group: data frame whose rows all belong to one group 
  #     e.g. all rows belong to one participant
  #   cols.today: column names of variables which we want to obtain
  #      past records of e.g. c("assessment.unixts","assessment.type")
  #   h: how far into the future we want to obtain records for each variable
  #      enumerated in cols.today; can accomodate differing lags
  #      e.g. c(2,5) if we want to obtain assessment time and assessment type
  #      in the next 2 and next 5 prompts
  #   this.numeric: array indicating which among cols.today is of numeric type
  #      e.g. c(TRUE, FALSE)
  # Output:
  #   df.this.group with number of new columns equal to the length of h
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  check.in <- cols.today %in% colnames(df.this.group)
  assert_that(sum(!(check.in))==0, msg = "cols.today must be column names in data")
  
  check.positive <- (is.numeric(h) & (h>0))
  assert_that(sum(!(check.positive))==0, msg = "elements of h must be positive")
  
  check.equal <- (length(h) == length(this.numeric))
  assert_that(isTRUE(check.equal), msg = "number of elements specified in h and this.numeric must be equal")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  for(i in 1:length(h)){
    new.col.name <- paste(cols.today[i], "_shift_plus_", h[i], sep="")
    
    if(isTRUE(this.numeric[i])){
      future.records <- tail(df.this.group[[cols.today[i]]], n=-h[i])
      future.records <- as.numeric(unlist(future.records))
      df.this.group[, new.col.name] <- c(future.records, rep(NA_real_,h[i]))
    }else{
      future.records <- tail(df.this.group[[cols.today[i]]], n=-h[i])
      future.records <- as.character(unlist(future.records))
      df.this.group[, new.col.name] <- c(future.records, rep(NA_character_,h[i]))
    }
  }
  
  df.this.group <- as.data.frame(df.this.group)
  
  return(df.this.group)
}

GetPastRecords <- function(df.this.group, cols.today, h, this.numeric){
  # About: Obtain past record of current value of a given column
  # 
  # Args:
  # 
  #   df.this.group: data frame whose rows all belong to one group 
  #     e.g. all rows belong to one participant
  #   cols.today: column names data of which we want to obtain past records
  #     e.g. c("assessment.unixts","assessment.type")
  #   h: how far back we want to obtain records
  #     can accomodate differing lengths of time
  #     e.g. c(1,3) if we want to obtain assessment time and assessment type 
  #     from the past 1 or 3 prompts
  #   this.numeric: array indicating which among cols.today is of numeric type
  #      e.g. c(TRUE, FALSE)
  # Output:
  #   df.this.group with number of new columns equal to the length of h
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  check.in <- cols.today %in% colnames(df.this.group)
  assert_that(sum(!(check.in))==0, msg = "cols.today must be column names in data")
  
  check.positive <- (is.numeric(h) & (h>0))
  assert_that(sum(!(check.positive))==0, msg = "elements of h must be positive")
  
  check.equal <- (length(h) == length(this.numeric))
  assert_that(isTRUE(check.equal), msg = "number of elements specified in h and this.numeric must be equal")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  
  for(i in 1:length(h)){
    new.col.name <- paste(cols.today[i], "_shift_minus_", h[i], sep="")
    
    if(isTRUE(this.numeric[i])){
      past.records <- head(df.this.group[[cols.today[i]]], n=-h[i])
      past.records <- as.numeric(unlist(past.records))
      df.this.group[, new.col.name] <- c(rep(NA_real_,h[i]), past.records)
    }else{
      past.records <- head(df.this.group[[cols.today[i]]], n=-h[i])
      past.records <- as.character(unlist(past.records))
      df.this.group[, new.col.name] <- c(rep(NA_character_,h[i]), past.records)
    }
    
  }
  
  df.this.group <- as.data.frame(df.this.group)
  
  return(df.this.group)
}

CleanLikertScale <- function(df, col.name){
  # About: Convert factor or character responses to numeric responses
  # Args:
  #   df: one individual's data frame
  #   col.name: name of a column in df where conversion will take place
  # Output:
  #   df with column col.name transformed into numeric format
  
  response.1 <- grepl("Strongly Disagree", as.character(df[,col.name]))
  response.2 <- grepl("Disagree", as.character(df[,col.name]))
  response.3 <- grepl("Neutral", as.character(df[,col.name]))
  response.4 <- grepl("Agree", as.character(df[,col.name]))
  response.5 <- grepl("Strongly Agree", as.character(df[,col.name]))
  
  v <- case_when(
    response.1 ~ 1,
    response.2 ~ 2,
    response.3 ~ 3,
    response.4 ~ 4,
    response.5 ~ 5,
    TRUE ~ NA_real_
  )
  df[,col.name] <- v
  
  return(df)
}

CleanNumericScale <- function(df, col.name){
  # About: Convert factor or character responses to numeric responses
  # Args:
  #   df: one individual's data frame
  #   col.name: name of a column in df where conversion will take place
  # Output:
  #   df with column col.name transformed into numeric format
  
  response.0 <- grepl("0", as.character(df[,col.name]))
  response.1 <- grepl("1", as.character(df[,col.name]))
  response.2 <- grepl("2", as.character(df[,col.name]))
  response.3 <- grepl("3", as.character(df[,col.name]))
  response.4 <- grepl("4", as.character(df[,col.name]))
  response.5 <- grepl("5", as.character(df[,col.name]))
  
  v <- case_when(
    response.0 ~ 0,
    response.1 ~ 1,
    response.2 ~ 2,
    response.3 ~ 3,
    response.4 ~ 4,
    response.5 ~ 5,
    TRUE ~ NA_real_
  )
  df[,col.name] <- v
  
  return(df)
}

