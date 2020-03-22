library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

CheckAnyResponse <- function(df, drop.cols){
  # About: Check whether there is a recorded response to each row of the 
  #   data frame df
  # Args:
  #   df: data frame containing raw data
  #   drop.cols: names of columns to drop from df prior to checking 
  #     whether any response was recorded in each row
  # Output:
  #   df with a new columns with.any.response: this variable is equal to 1
  #   if there is a record of at least one question completed and 0 if there
  #   is no record of any question completed
  
  # ---------------------------------------------------------------------------
  # Check validity of inputs
  # ---------------------------------------------------------------------------
  assert_that(is.character(drop.cols), msg = "drop.cols must be a character array")
  
  # ---------------------------------------------------------------------------
  # Begin tasks
  # ---------------------------------------------------------------------------
  df.items <- df %>% select(-drop.cols)
  
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

CountWithin <- function(df.this.group, ones.col, current.ts, H){
  # About: ones.col is a column indicating which rows satisfy
  #   some condition (i.e. equal to 1 if condition is satisfied). 
  #   and equal to 0 if the condition is not satisfied. 
  #   The number of rows with ones.col=1 in the past H hours from
  #   current.ts are counted.
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  # Output:
  #   An array of counts with number of rows equal to 
  #   number of current.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.counts <- list()
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>% 
      filter((time.unixts.scaled >= LB) & (time.unixts.scaled <= UB))
    
    counts <- nrow(df.subset)
    list.counts <- append(list.counts, counts)
  }
  vec.counts <- do.call(rbind, list.counts)
  vec.counts <- as.data.frame(vec.counts)
  
  return(vec.counts)
}

MeanWithin <- function(df.this.group, ones.col, current.ts, H, this.var){
  # About: ones.col is a column indicating which rows satisfy
  #   some condition (i.e. equal to 1 if condition is satisfied). 
  #   and equal to 0 if the condition is not satisfied. 
  #   The number of rows with ones.col=1 in the past H hours from
  #   current.ts are used to obtain the mean of this.var.
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  # Output:
  #   An array of means with number of rows equal to 
  #   number of current.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.quantity <- list()
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>%
      filter((.data[[current.ts]] >= LB) & (.data[[current.ts]] <= UB))
    
    check.missing <- sum(is.na(df.subset[,this.var]))
    if(check.missing == nrow(df.subset)){
      quantity <- NA
    }else{
      these.vals <- df.subset[,this.var]
      these.vals <- as.matrix(these.vals)
      quantity <- mean(these.vals, na.rm=TRUE)
    }
    
    list.quantity <- append(list.quantity, quantity)
  }
  vec.quantity <- do.call(rbind, list.quantity)
  vec.quantity <- as.data.frame(vec.quantity)
  
  return(vec.quantity)
}

VarianceWithin <- function(df.this.group, ones.col, current.ts, H, this.var){
  # About: ones.col is a column indicating which rows satisfy
  #   some condition (i.e. equal to 1 if condition is satisfied). 
  #   and equal to 0 if the condition is not satisfied. 
  #   The number of rows with ones.col=1 in the past H hours from
  #   current.ts are used to obtain the variance of this.var.
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  # Output:
  #   An array of variances with number of rows equal to 
  #   number of current.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.quantity <- list()
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>% 
      filter((.data[[current.ts]] >= LB) & (.data[[current.ts]] <= UB))
    
    check.missing <- sum(is.na(df.subset[,this.var]))
    if(check.missing == nrow(df.subset)){
      quantity <- NA
    }else{
      these.vals <- df.subset[,this.var]
      these.vals <- as.matrix(these.vals)
      quantity <- var(these.vals, na.rm=TRUE)
    }
    
    list.quantity <- append(list.quantity, quantity)
  }
  vec.quantity <- do.call(rbind, list.quantity)
  vec.quantity <- as.data.frame(vec.quantity)
  
  return(vec.quantity)
}

MaxWithin <- function(df.this.group, ones.col, current.ts, H, this.var){
  # About: ones.col is a column indicating which rows satisfy
  #   some condition (i.e. equal to 1 if condition is satisfied). 
  #   and equal to 0 if the condition is not satisfied. 
  #   The number of rows with ones.col=1 in the past H hours from
  #   current.ts are used to obtain the maximum of this.var.
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  # Output:
  #   An array of max values with number of rows equal to 
  #   number of current.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.quantity <- list()
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>% 
      filter((.data[[current.ts]] >= LB) & (.data[[current.ts]] <= UB))
    
    check.missing <- sum(is.na(df.subset[,this.var]))
    if(check.missing == nrow(df.subset)){
      quantity <- NA
    }else{
      these.vals <- df.subset[,this.var]
      these.vals <- as.matrix(these.vals)
      quantity <- max(these.vals, na.rm=TRUE)
    }
    
    list.quantity <- append(list.quantity, quantity)
  }
  vec.quantity <- do.call(rbind, list.quantity)
  vec.quantity <- as.data.frame(vec.quantity)
  
  return(vec.quantity)
}

MinWithin <- function(df.this.group, ones.col, current.ts, H, this.var){
  # About: ones.col is a column indicating which rows satisfy
  #   some condition (i.e. equal to 1 if condition is satisfied). 
  #   and equal to 0 if the condition is not satisfied. 
  #   The number of rows with ones.col=1 in the past H hours from
  #   current.ts are used to obtain the minimum of this.var.
  # Args: 
  #   df.this.group: a data frame corresponding groups in the
  #   data for which this counting operation is to be performed
  #   (e.g. each group may be 1 participant)
  # Output:
  #   An array of min values with number of rows equal to 
  #   number of current.ts entries in df.this.group
  
  df.this.group[,"pastH.ts"] <- df.this.group[,current.ts] - H*60*60
  list.quantity <- list()
  # For each row of df.this.group
  for(i in 1:nrow(df.this.group)){
    LB <- as.numeric(df.this.group[i,"pastH.ts"])
    UB <- as.numeric(df.this.group[i,current.ts])
    
    df.subset <- df.this.group %>% 
      filter((.data[[current.ts]] >= LB) & (.data[[current.ts]] <= UB))
    
    check.missing <- sum(is.na(df.subset[,this.var]))
    if(check.missing == nrow(df.subset)){
      quantity <- NA
    }else{
      these.vals <- df.subset[,this.var]
      these.vals <- as.matrix(these.vals)
      quantity <- min(these.vals, na.rm=TRUE)
    }
    
    list.quantity <- append(list.quantity, quantity)
  }
  vec.quantity <- do.call(rbind, list.quantity)
  vec.quantity <- as.data.frame(vec.quantity)
  
  return(vec.quantity)
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
    new.col.name <- paste(cols.today[i], "_shift.plus.", h[i], sep="")
    future.records <- tail(df.this.group[, cols.today[i]], n=-h[i])
    
    if(isTRUE(this.numeric[i])){
      past.records <- as.numeric(unlist(past.records))
      df.this.group[, new.col.name] <- c(rep(NA_real_,h[i]), past.records)
    }else{
      past.records <- as.character(unlist(past.records))
      df.this.group[, new.col.name] <- c(rep(NA_character_,h[i]), past.records)
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
    new.col.name <- paste(cols.today[i], "_shift.minus.", h[i], sep="")
    past.records <- head(df.this.group[, cols.today[i]], n=-h[i])
    
    if(isTRUE(this.numeric[i])){
      past.records <- as.numeric(unlist(past.records))
      df.this.group[, new.col.name] <- c(rep(NA_real_,h[i]), past.records)
    }else{
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

