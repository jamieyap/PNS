# List to collect item responses
list.resp.cc1 <- list()
N <- length(list.df.raw.smoking.DATA.cc1)

for(i in 1:N){
  df.raw <- list.df.raw.smoking.DATA.cc1[[i]]
  
  all.col.names <- colnames(df.raw)
  check <- (("question_answers_0_response_0") %in% all.col.names)
  
  if(isTRUE(check)){
    list.items <- list()
    # There are 54 items in a Random EMA for CC1 participants
    # The last item is a thank you message
    # Item numbering begins at 0: 0, 1, ..., 53
    # Thus, idx ranges from 0 to 52
    for(idx in collect.idx){  
      these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
      these.col.names <- all.col.names[these.col.names]
      df.item <- df.raw %>% select(these.col.names) 
      
      # Note: Column names from CC1 to CC2 changed
      # For each of the EMA items, get:
      # question response: question_answers_idx_response_0
      idx.resp <- match(x = paste("question_answers_",idx,"_response_0", sep=""), table = these.col.names)
      
      if(!is.na(idx.resp)){
        # Collect info altogether
        question.resp <- df.item[,idx.resp]
        question.resp <- as.character(question.resp)
        question.resp <- replace(question.resp, question.resp=="", NA_character_)
        question.resp <- as.matrix(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
        
      }else{
        # This column name occurs when there is no recorded response to this EMA item
        # at any point during the data collection period. This can occur when
        # all EMAs prompted are either MISSED or ABANDONED_BY_TIMEOUT
        #idx.resp <- match(x = paste("question_answers_",idx,"_response", sep=""), table = these.col.names)
        
        question.resp <- rep(NA_character_, times = nrow(df.item))
        question.resp <- as.matrix(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }
    }  # End looping through collect.idx
    
    df.resp <- do.call(cbind, list.items)
    df.resp <- as.data.frame(df.resp)
    df <- df.raw %>% select(user.id, merge.id)
    df <- cbind(df, df.resp)
    list.resp.cc1 <- append(list.resp.cc1, list(df))
    
  }else{
    df.resp <- rep(NA, nrow(df.raw)*length(collect.idx))
    df.resp <- matrix(df.resp, ncol = length(collect.idx))
    df.resp <- as.data.frame(df.resp)
    colnames(df.resp) <- paste("item.",collect.idx, sep="")
    df <- df.raw %>% select(user.id, merge.id)
    df <- cbind(df, df.resp) 
    list.resp.cc1 <- append(list.resp.cc1, list(df))
  }
}

df.resp.cc1 <- do.call(rbind, list.resp.cc1)

