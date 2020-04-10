# List to collect item responses
list.resp.cc2 <- list()
N <- length(list.df.raw.random.DATA.cc2)

for(i in 1:N){
  df.raw <- list.df.raw.random.DATA.cc2[[i]]
  
  all.col.names <- colnames(df.raw)
  check <- (("questions_0_response_0") %in% all.col.names)
  
  if(isTRUE(check)){
    list.items <- list()
    # There are 64 items in a Random EMA for CC2 participants
    # The last item is a thank you message
    # Thus, idx ranges from 1 to 63
    for(idx in collect.idx){  
      these.col.names <- grepl(paste("_",idx,"_",sep=""), all.col.names)
      these.col.names <- all.col.names[these.col.names]
      df.item <- df.raw %>% select(these.col.names) 
      
      # Note: Column names from CC1 to CC2 changed
      # For each of the EMA items, get:
      # question response: question_idx_response_0
      idx.resp <- match(x = paste("questions_",idx,"_response_0", sep=""), table = these.col.names)
      
      if(!is.na(idx.resp)){
        # Collect info into one data frame
        question.resp <- df.item[,idx.resp]
        question.resp <- as.character(question.resp)
        question.resp <- replace(question.resp, question.resp=="", NA_character_)
        question.resp <- as.matrix(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }else{
        question.resp <- rep(NA_character_, times = nrow(df.item))
        question.resp <- as.matrix(question.resp)
        colnames(question.resp) <- paste("item.",idx, sep="")
        list.items <- append(list.items,list(question.resp))
      }
    }
    
    df.resp <- do.call(cbind, list.items)
    df.resp <- as.data.frame(df.resp)
    df <- df.raw %>% select(user.id, merge.id)
    df <- cbind(df, df.resp)
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }else{
    df.resp <- rep(NA, nrow(df.raw)*length(collect.idx))
    df.resp <- matrix(df.resp, ncol = length(collect.idx))
    df.resp <- as.data.frame(df.resp)
    colnames(df.resp) <- paste("item.",collect.idx, sep="")
    df <- df.raw %>% select(user.id, merge.id)
    df <- cbind(df, df.resp) 
    list.resp.cc2 <- append(list.resp.cc2, list(df))
  }
}

df.resp.cc2 <- do.call(rbind, list.resp.cc2)

