optimization_individuals <- function(households_set, individuals_set,
                                     ids1,ids2,top_houses,dist_prob_ind,data1,
                                     data2, matches_ind){


optimization_solution <- data.frame(Index2014 = as.numeric(),
                                    Index2016 = as.numeric(),
                                    Correct_Match = as.numeric(),
                                    House2016 = as.numeric(),
                                    House2014 = as.numeric(),
                                    Ind2016 = as.numeric(),
                                    Correct = as.numeric()
)

for(i in 1:length(households_set)){ ## households_set = variavel que seleciona as households para test set
  print(i)
  pos <- households_set[i]
  house <- ids1$Household[pos]
  house_id <- which(top_houses$Household_A == house)
  houses2016 <- as.numeric(top_houses$Household_B[house_id])
  
  i_range = unlist(ids1[pos,1])
  j_range = unlist(ids2$Index[which(ids2$Household == houses2016)])
  
  distance_individuals_prob <- dist_prob_ind[i_range,j_range]
  M <- distance_individuals_prob
  if(!is.null(nrow(M))){
    Mnew <- matrix(mean(M), nrow(M)+ncol(M), nrow(M)+ncol(M))
    Mnew[1:nrow(M), 1:ncol(M)] <- M
    fit <- lp.assign(Mnew, direction = "max")
    solution <- fit$solution[1:nrow(M), 1:ncol(M)]*M
  } else {
    Mnew <- matrix(mean(M), 1+length(M), 1+length(M))
    Mnew[1:1, 1:length(M)] <- M
    fit <- lp.assign(Mnew, direction = "max")
    solution <- fit$solution[1:1, 1:length(M)]*M
  }
  
  for(l in 1:length(i_range)){
    pos2014 <-  i_range[l]
    pos2 <- which(matches_ind[,1] == data1$IND[pos2014])
    pos_train <- which(individuals_set == data1$IND[pos2014])
    optimization_solution[pos_train,1] <- pos2014
    optimization_solution[pos_train,5] <- house
    optimization_solution[pos_train,3] <-  ifelse(length(pos2) == 0, NA, matches_ind[pos2,2])
    
    pos2016 <- ifelse(is.null(nrow(solution)),which(solution > 0),which(solution[l,] > 0))
    if(!is.na(pos2016)){
      optimization_solution[pos_train,2] <- j_range[pos2016]
      optimization_solution[pos_train,4] <- data2$NQUEST[j_range[pos2016]]
      optimization_solution[pos_train,6] <- data2$IND[j_range[pos2016]]
    } else optimization_solution[pos_train,2] <- NA
    
    if(is.na(optimization_solution[pos_train,3])){
      if(is.na(optimization_solution[pos_train,2]))
        optimization_solution[pos_train,7] <- 1 
      else optimization_solution[pos_train,7] <- 0
    } else {
      test <- ifelse(is.na(optimization_solution[pos_train,6] == optimization_solution[pos_train,3]),FALSE,
                     optimization_solution[pos_train,6] == optimization_solution[pos_train,3])
      if(test == TRUE) 
        optimization_solution[pos_train,7] <- 1
      else optimization_solution[pos_train,7] <- 0
    }
  }
}

optimization_solution$Correct_cat <- NA

for(i in 1:nrow(optimization_solution)){
  true_match <- optimization_solution$Correct_Match[i]
  match_found <- optimization_solution$Ind2016[i]
  if(!is.na(true_match) & is.na(match_found)){
    optimization_solution$Correct[i] <- 0
    optimization_solution$Correct_cat[i] <- "MNF"
  } else if(!is.na(true_match) & !is.na(match_found)){
    optimization_solution$Correct[i] <- ifelse(true_match == match_found,1,0)
    optimization_solution$Correct_cat[i] <- ifelse(true_match == match_found,"CM","WM")
  } else if(is.na(true_match) & is.na(match_found)){
    optimization_solution$Correct[i]  <- 1
    optimization_solution$Correct_cat[i] <- "CNM"
  } else {
    optimization_solution$Correct[i]  <- 0
    optimization_solution$Correct_cat[i] <- "WNM"
  }
}

return(optimization_solution)
}

optimization_individuals_2 <- function(households_set, individuals_set,
                                       ids1,ids2,dist_prob_ind,data1,
                                       data2, matches_ind, households_matches){
  
  
  optimization_solution <- data.frame(Index2014 = as.numeric(),
                                      Index2016 = as.numeric(),
                                      Correct_Match = as.numeric(),
                                      House2016 = as.numeric(),
                                      House2014 = as.numeric(),
                                      Ind2016 = as.numeric(),
                                      Correct = as.numeric()
  )
  
  for(i in 1:length(households_set)){ ## households_set = variavel que seleciona as households para test set
    #print(i)
    pos <- households_set[i]
    house <- ids1$Household[pos]
    house_id <- which(households_matches$Household_A == house)
    houses2016 <- as.numeric(households_matches$Household_B[house_id])
    
    if(is.na(houses2016)){
      i_range = unlist(ids1[pos,1])
      for(l in 1:length(i_range)){
        pos2014 <-  i_range[l]
        pos2 <- which(matches_ind[,1] == data1$IND[pos2014])
        pos_train <- which(individuals_set == data1$IND[pos2014])
        optimization_solution[pos_train,1] <- pos2014
        optimization_solution[pos_train,5] <- house
        optimization_solution[pos_train,3] <- ifelse(length(pos2) == 0, NA, matches_ind[pos2,2])
        
        optimization_solution[pos_train,2] <- NA
        optimization_solution[pos_train,4] <- NA
        optimization_solution[pos_train,6] <- NA
        
        if(is.na(optimization_solution[pos_train,3])){
          if(is.na(optimization_solution[pos_train,2]))
            optimization_solution[pos_train,7] <- 1 
          else optimization_solution[pos_train,7] <- 0
        } else {
          test <- ifelse(is.na(optimization_solution[pos_train,6] == optimization_solution[pos_train,3]),FALSE,
                         optimization_solution[pos_train,6] == optimization_solution[pos_train,3])
          if(test == TRUE) 
            optimization_solution[pos_train,7] <- 1
          else optimization_solution[pos_train,7] <- 0
        }
      }
      
    } else {
      
      i_range = unlist(ids1[pos,1])
      j_range = unlist(ids2$Index[which(ids2$Household == houses2016)])
      
      distance_individuals_prob <- dist_prob_ind[i_range,j_range]
      M <- distance_individuals_prob
      if(!is.null(nrow(M))){
        Mnew <- matrix(mean(M), nrow(M)+ncol(M), nrow(M)+ncol(M))
        Mnew[1:nrow(M), 1:ncol(M)] <- M
        fit <- lp.assign(Mnew, direction = "max")
        solution <- fit$solution[1:nrow(M), 1:ncol(M)]*M
      } else {
        Mnew <- matrix(mean(M), 1+length(M), 1+length(M))
        Mnew[1:1, 1:length(M)] <- M
        fit <- lp.assign(Mnew, direction = "max")
        solution <- fit$solution[1:1, 1:length(M)]*M
      }
      
      for(l in 1:length(i_range)){
        pos2014 <-  i_range[l]
        pos2 <- which(matches_ind[,1] == data1$IND[pos2014])
        pos_train <- which(individuals_set == data1$IND[pos2014])
        optimization_solution[pos_train,1] <- pos2014
        optimization_solution[pos_train,5] <- house
        optimization_solution[pos_train,3] <-  ifelse(length(pos2) == 0, NA, matches_ind[pos2,2])
        
        pos2016 <- ifelse(is.null(nrow(solution)),which(solution > 0),which(solution[l,] > 0))
        if(!is.na(pos2016)){
          optimization_solution[pos_train,2] <- j_range[pos2016]
          optimization_solution[pos_train,4] <- data2$NQUEST[j_range[pos2016]]
          optimization_solution[pos_train,6] <- data2$IND[j_range[pos2016]]
        } else optimization_solution[pos_train,2] <- NA
        
        if(is.na(optimization_solution[pos_train,3])){
          if(is.na(optimization_solution[pos_train,2]))
            optimization_solution[pos_train,7] <- 1 
          else optimization_solution[pos_train,7] <- 0
        } else {
          test <- ifelse(is.na(optimization_solution[pos_train,6] == optimization_solution[pos_train,3]),FALSE,
                         optimization_solution[pos_train,6] == optimization_solution[pos_train,3])
          if(test == TRUE) 
            optimization_solution[pos_train,7] <- 1
          else optimization_solution[pos_train,7] <- 0
        }
      }
    }
  }
  
  optimization_solution$Correct_cat <- NA
  
  for(i in 1:nrow(optimization_solution)){
    true_match <- optimization_solution$Correct_Match[i]
    match_found <- optimization_solution$Ind2016[i]
    if(!is.na(true_match) & is.na(match_found)){
      optimization_solution$Correct[i] <- 0
      optimization_solution$Correct_cat[i] <- "MNF"
    } else if(!is.na(true_match) & !is.na(match_found)){
      optimization_solution$Correct[i] <- ifelse(true_match == match_found,1,0)
      optimization_solution$Correct_cat[i] <- ifelse(true_match == match_found,"CM","WM")
    } else if(is.na(true_match) & is.na(match_found)){
      optimization_solution$Correct[i]  <- 1
      optimization_solution$Correct_cat[i] <- "CNM"
    } else {
      optimization_solution$Correct[i]  <- 0
      optimization_solution$Correct_cat[i] <- "WNM"
    }
  }
  
  return(optimization_solution)
}
