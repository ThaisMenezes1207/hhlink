basic_variables <- function(data, years){
  
  range1 <- which(data[,1] == years[1])
  range2 <- which(data[,1] == years[2])
  
  lim1 <- length(unique(data[range1,2]))
  lim2 <- length(unique(data[range2,2]))
  range_h1 <- c(1:(lim1)) 
  range_h2 <- c((lim1+1):(lim1+lim2))
  
  id <- data.frame(Index = as.numeric(),
                   Household = as.numeric())

  
  #### RANGE YEAR 1
  ids1 <- unique(data[range1,2])
  range_list_1 <- list()
  for(i in 1:length(ids1)){
    range <- range1[which(data[range1,2] == ids1[i])]
    range_list_1[[i]] <- range
  }
  
  #### RANGE YEAR 2
  ids2 <- unique(data[range2,2])
  range_list_2 <- list()
  for(i in 1:length(ids2)){
    range <- range2[which(data[range2,2] == ids2[i])]
    range_list_2[[i]] <- range
  }
  
  range_list <- c(range_list_1,range_list_2)
  ids <- c(ids1,ids2)
  
  for (i in 1:length(range_list)){
    frame = data.frame(I(range_list[i]),ids[i])
    names(frame) = c("Index","Household")
    id <- rbind(id,frame)
  }
  
  return(list(range1 = range1,range2 = range2, range_h1 = range_h1,
              range_h2 = range_h2, id = id))
  
}

