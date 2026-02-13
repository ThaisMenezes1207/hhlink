###########
# PACKAGES
###########
require(optimx)
require(fastmatch)
require(caret)
require(PRROC)
require(cvAUC)
require(Rfast)
require(glmnet)
require(lpSolve)

###########
# FUNCTION
###########

source("Basic variables.R") ## Contains functions that are constantly used to set some basic information of the data
source("LinearProgrammingIndividuals.R") ## Contains the functions used to run the linear programming optimization

linear_combinantion <- function(distances_matrix_features,beta) {
  dist_matrix <-  matrix(0,nrow = nrow(distances_matrix_features[[1]]), ncol = ncol(distances_matrix_features[[1]]))
  for(k in 1:length(distances_matrix_features)){
    dist_matrix <- dist_matrix + beta[k+1]*distances_matrix_features[[k]]
  }
  return(dist_matrix)
}

create_distance_households_2 <- function(distances_matrix_features,paired_houses,beta,
                                         household2014_id,household2016_id,pos2014,pos2016){
  
  dist_matrix <- linear_combinantion(distances_matrix_features,beta)
  dist_matrix <- dist_matrix[pos2014,pos2016]
  house2014 <- unique(paired_houses[,1])
  house2016 <- unique(paired_houses[,2])
  
  col_max <- matrix(NA,nrow = length(house2014), ncol = length(house2016))
  for(s in 1:length(house2014)){
    rows <- which(household2014_id == house2014[s])
    matrix <- as.matrix(dist_matrix[rows,])
    
    if(length(rows) == 1){
      household_v <- matrix
    } else {
      household_v <- colMins(matrix, value = TRUE)
    }
    col_max[s,] <- ctapply(household_v, household2016_id, max)
  }
  
  row_max <- matrix(NA,nrow = length(house2014), ncol = length(house2016))
  for(t in 1:length(house2016)){
    cols <- which(household2016_id == house2016[t])
    matrix <- as.matrix(dist_matrix[,cols])
    
    if(length(cols) == 1){
      household_v <- matrix
    } else {
      household_v <- rowMins(matrix, value = TRUE)
    }
    row_max[,t] <- ctapply(household_v, household2014_id, max)
  }
  
  hausdorff <- matrix(NA,nrow = length(house2014), ncol = length(house2016))
  hausdorff <- (1/2)*((col_max + row_max) + abs((col_max - row_max)))
  
  households_dist <- c(t(hausdorff))
  
  return(households_dist)
} ## Compute the Hausdorff distance between two households

probability <- function(households_dist,beta){
  probability <- NULL
  delta <- beta[1] - households_dist
  
  probability <- exp(delta)/(1+exp(delta))
  return(probability)
} ## Computes the probability of match based on the estimated parameters

loglik <- function(distances_matrix_features,paired_houses,lambda,beta,household2014_id,household2016_id,pos2014,pos2016){
  
  probability_c <- probability(create_distance_households_2(distances_matrix_features,paired_houses,beta,household2014_id,household2016_id,pos2014,pos2016),beta)
  
  loglikelihood <- (paired_houses[,3]*log(probability_c)) + 
    ((1-paired_houses[,3])*log((1-probability_c))) 
  
  l <- sum(loglikelihood) - lambda*sum((beta[-1])^2)
  return(l) 
} ## Computes the log-likelihood

create_distance_households_matrix <- function(distances_matrix_features,paired_houses,beta,
                                              household2014_id,household2016_id,pos2014,pos2016){
  
  dist_matrix <- linear_combinantion(distances_matrix_features,beta)
  dist_matrix <- dist_matrix[pos2014,pos2016]
  house2014 <- unique(paired_houses[,1])
  house2016 <- unique(paired_houses[,2])
  
  col_max <- matrix(NA,nrow = length(house2014), ncol = length(house2016))
  for(s in 1:length(house2014)){
    rows <- which(household2014_id == house2014[s])
    matrix <- as.matrix(dist_matrix[rows,])
    
    if(length(rows) == 1){
      household_v <- matrix
    } else {
      household_v <- colMins(matrix, value = TRUE)
    }
    col_max[s,] <- ctapply(household_v, household2016_id, max)
  }
  
  row_max <- matrix(NA,nrow = length(house2014), ncol = length(house2016))
  for(t in 1:length(house2016)){
    cols <- which(household2016_id == house2016[t])
    matrix <- as.matrix(dist_matrix[,cols])
    
    if(length(cols) == 1){
      household_v <- matrix
    } else {
      household_v <- rowMins(matrix, value = TRUE)
    }
    row_max[,t] <- ctapply(household_v, household2014_id, max)
  }
  
  hausdorff <- matrix(NA,nrow = length(house2014), ncol = length(house2016))
  hausdorff <- (1/2)*((col_max + row_max) + abs((col_max - row_max)))
  
  return(hausdorff)
} ## Compute the Hausdorff distance between two households


probability_matrix <- function(hausdorff_matrix,beta){
  probability_m <- matrix(NA,nrow = nrow(hausdorff_matrix),ncol = ncol(hausdorff_matrix))
  
  delta <- beta[1] - hausdorff_matrix
  
  
  probability_m <- exp(delta)/(1+exp(delta))
  return(probability_m)
} ## Compute the probability of a match - MATRIX

probability_matrix_ind <- function(ind_prob_matrix,beta){
  probability_m <- matrix(NA,nrow = nrow(ind_prob_matrix),ncol = ncol(ind_prob_matrix))
  
  delta <- beta[1] + ind_prob_matrix
  
  
  probability_m <- exp(delta)/(1+exp(delta))
  return(probability_m)
} ## Compute the probability of a match between individuals - MATRIX

topk_house <- function(ids1,ids2,dist_prob,k){
  top_index <- NULL
  top_houses <- data.frame(Household_A = as.numeric(),
                           Household_B = as.character(),
                           Range_B = as.character())
  top_houses[1,1] <- NA
  
  for (i in 1:nrow(ids1)){
    probs <- dist_prob[i,]
    top_prob <- round(sort(probs, decreasing = TRUE)[1:k],4)
    pos <- NULL
    for(j in 1:k){
      pos_a <-  which(round(probs,4) == top_prob[j]) 
      pos <- c(pos,pos_a) 
    }
    top_index[1:k] <- unique(pos)[1:k]
    
    top_houses[i,1] <- ids1[i,2]
    top_houses[i,2] <- paste(ids2[top_index,2],collapse = ",")
    top_houses[i,3] <- paste(top_index,collapse = ",")
    
  }
  
  return(top_houses)
} ## Selects the top k households (the k households with the highest probability of a match)

###########
# DATA - TRAIN
###########
data <- readRDS("Data Italy - Final") ## Original file
data1 <- data[which(data$Year == 2014),]
data2 <- data[which(data$Year == 2016),]

matches <- readRDS("Matches") ## File with the TRUE matches between households
matches_ind <- readRDS("Matches Ind") ## File with the TRUE matches between individuals
paired_houses <- readRDS("Pair Household - Complete") ## All possible pairs of households in the data
data_mod_individuals <- readRDS("data_mod_individuals_Italy (House with match only)") ## Data containing all the individuals information for the individual model training

###########
# BASIC VARIABLES
###########
basic <- basic_variables(data,years = c(2014,2016))

range2014_ind <- basic$range1
range2016_ind <- basic$range2

range2014_h <- basic$range_h1
range2016_h <- basic$range_h2

range2016_h_2 <- range2016_h - length(basic$range_h1)

id <- basic$id

n <- nrow(data[data$Year == 2014,])
ids1 <- id[1:length(range2014_h),]
ids2 <- id[c((length(range2014_h)+1):nrow(id)),]
for(j in 1:nrow(ids2)){
  range2 <- unlist(ids2$Index[j])-n
  ids2$Index[[j]] <- I(range2)
}

household2014_id <- NULL
for(i in 1:nrow(ids1)){
  a <- rep(ids1$Household[i],length(unlist(ids1$Index[i])))
  household2014_id <- c(household2014_id,a)
}

household2016_id <- NULL
for(i in 1:nrow(ids2)){
  a <- rep(ids2$Household[i],length(unlist(ids2$Index[i])))
  household2016_id <- c(household2016_id,a)
}


distances_matrix_features <- readRDS("distance_matrix_features")
distances_matrix_features[[9]] <- NULL ## one extra distance computed by combining two variables into one but that is not used 

pos2014 <- c(1:nrow(data1))
pos2016 <- c(1:nrow(data2))

###########
# DATA - TEST
###########
setwd("~/2016-2020") ## Changing directory to read the data from 2016 and 2020 
# Data files has the same name so it is crucial to change directory!!
data_test <- readRDS("Data Italy - Final")
data1_test <- data_test[which(data_test$Year == 2016),]
data2_test <- data_test[which(data_test$Year == 2020),]

matches_test <- readRDS("Matches")
matches_ind_test <- readRDS("Matches Ind")
paired_houses_test <- readRDS("Pair Household - Complete")

###########
# BASIC VARIABLES
###########
basic <- basic_variables(data_test,years = c(2016,2020))

range2014_ind_test <- basic$range1
range2016_ind_test <- basic$range2

range2014_h_test <- basic$range_h1
range2016_h_test <- basic$range_h2

id_test <- basic$id

n_test <- nrow(data_test[data_test$Year == 2016,])
ids1_test <- id_test[1:length(range2014_h_test),]
ids2_test <- id_test[c((length(range2014_h_test)+1):nrow(id_test)),]
for(j in 1:nrow(ids2_test)){
  range2 <- unlist(ids2_test$Index[j])-n_test
  ids2_test$Index[[j]] <- I(range2)
}

household2016_id_test <- NULL
for(i in 1:nrow(ids1_test)){
  a <- rep(ids1_test$Household[i],length(unlist(ids1_test$Index[i])))
  household2016_id_test <- c(household2016_id_test,a)
}

household2020_id_test <- NULL
for(i in 1:nrow(ids2_test)){
  a <- rep(ids2_test$Household[i],length(unlist(ids2_test$Index[i])))
  household2020_id_test <- c(household2020_id_test,a)
}

distances_matrix_features_test <- readRDS("distance_matrix_features") ## directory was changed so this is corresponding to 2016 and 2020 databases
distances_matrix_features_test[[9]] <- NULL ## one extra distance computed by combining two variables into one but that is not used 

pos2016_test <- c(1:nrow(data1_test))
pos2020_test <- c(1:nrow(data2_test))


#################################
# HOUSEHOLD MODEL
#################################

###########
# TRAIN
###########

beta <- c(3.12,1.85,1.83,4.75,0.15,0.44,1.86,1.76,1) ## Initial values! 

### NO REGULARIZARION

system.time(
  op.result <- optimx(par = beta,
                      fn = loglik,
                      paired_houses = paired_houses,
                      household2014_id = household2014_id,
                      household2016_id = household2016_id,
                      pos2014 = pos2014,
                      pos2016 = pos2016,
                      distances_matrix_features = distances_matrix_features,
                      lambda = 0,
                      method = "L-BFGS-B",
                      lower = c(-Inf,0,0,0,0,0,0,0,0),
                      #itnmax=c(1000),
                      control=list(maximize=TRUE)
  )
)


round(unlist(op.result),3)
saveRDS(op.result,"op.result_Households_aproach3")

###########
# PERFORMANCE TRAIN X TEST
###########

#op.result <- readRDS("op.result - Households - aproach3")
beta_f <- unlist(op.result)[1:9]

## AUC-PR TRAIN DATA 
hausdorff_matrix_train <- create_distance_households_matrix(distances_matrix_features,paired_houses,beta_f,
                                                            household2014_id,household2016_id,pos2014, pos2016)
dim(hausdorff_matrix_train) 
probability_matrix_train <- probability_matrix(hausdorff_matrix_train,beta_f)
saveRDS(probability_matrix_train,"HouseholdProbMatrix_2014_2016")

## AUC-PR TEST DATA
hausdorff_matrix_test <- create_distance_households_matrix(distances_matrix_features_test,paired_houses_test,beta_f,
                                                           household2016_id_test,household2020_id_test,pos2016_test, pos2020_test)
dim(hausdorff_matrix_test) 
probability_matrix_test <- probability_matrix(hausdorff_matrix_test,beta_f)

###########
# TRUE MATCH POSITION
###########

## TRAIN DATA
house2014 <- unique(paired_houses[,1])
house2016 <- unique(paired_houses[,2])
match_pos_ordered <- NULL
for(i in 1:nrow(probability_matrix_train)){
  household_2014 <- house2014[i] 
  
  match_pos <- which(house2016 == matches[which(matches[,1] == household_2014),2])
  
  if( length(match_pos) > 0){
    a <- rank(-probability_matrix_train[i,],ties.method = "first")[match_pos]
  } 
  
  match_pos_ordered <- c(match_pos_ordered,a)
  
}

saveRDS(match_pos_ordered, "MatchPos_Train_2014_2016")

## TEST DATA
house2016 <- unique(paired_houses_test[,1])
house2020 <- unique(paired_houses_test[,2])
match_pos_ordered <- NULL
for(i in 1:nrow(probability_matrix_test)){
  household_2016 <- house2016[i] 
  
  match_pos <- which(house2020 == matches_test[which(matches_test[,1] == household_2016),2])
  
  if( length(match_pos) > 0){
    a <- rank(-probability_matrix_test[i,],ties.method = "first")[match_pos]
  } 
  
  match_pos_ordered <- c(match_pos_ordered,a)
  
}

saveRDS(match_pos_ordered, "MatchPos_Test_2016_2020")

#################################
# INDIVIDUAL MODEL
#################################

x = as.matrix(data_mod_individuals[,3:10])
y = data_mod_individuals$MATCH
set.seed(12345)
mod <- cv.glmnet(x, y, family = "binomial", type.measure = "default",
                 alpha = 0, upper.limits = 0)

round(coef(mod,  s = "lambda.min"),3)

saveRDS(mod,"modIndividuals_approach3")

#################################
# THRESHOLD ANALYSIS - HOUSEHOLD
#################################

threshold <- seq(0,0.5,by = 0.01)
threshold <- sort(threshold,decreasing = TRUE)

probability_matrix_train <- probability_matrix_train ## or readRDS("HouseholdProbMatrix_2014_2016") as this was saved! 
pos_max <- apply(probability_matrix_train, 1, which.max)

match_data_train <- matches
prop_matches <- nrow(na.omit(match_data_train))/nrow(match_data_train)

house2014 <- unique(paired_houses$Household2014)
house2016 <- unique(paired_houses$Household2016)

prop <- 0
t = 1
threshold_test <- threshold[t]
while(prop < prop_matches){
  match_matrix_train <- matrix(0,nrow = nrow(probability_matrix_train), ncol = ncol(probability_matrix_train))
  for (i in 1:nrow(match_matrix_train)) {
    match_matrix_train[i,pos_max[i]] <- ifelse(probability_matrix_train[i,pos_max[i]] >= threshold_test,1,0)
  }
  threshold_final_est <- threshold_test
  prop <- sum(rowSums(match_matrix_train))/nrow(match_matrix_train)
  t <- t+1
  threshold_test <- threshold[t]
}
threshold_final <- threshold_final_est

household_matches_train <- data.frame()
for (i in 1:nrow(match_matrix_train)) {
  household_matches_train[i,1] <- house2014[i]
  pos_match <- which(match_matrix_train[i,] > 0)
  household_matches_train[i,2] <- ifelse(length(pos_match) == 0, NA, house2016[pos_match])
}
names(household_matches_train) <- c("Household_A","Household_B")
saveRDS(household_matches_train,"household_matches_2014_2016_threshold")

probability_matrix_test <- readRDS("HouseholdProbMatrix_2016_2020")
pos_max <- apply(probability_matrix_test, 1, which.max)

house2016 <- unique(paired_houses_test$Household2016)
house2020 <- unique(paired_houses_test$Household2020)

match_matrix_test <- matrix(0,nrow = nrow(probability_matrix_test), ncol = ncol(probability_matrix_test))
for (i in 1:nrow(match_matrix_test)) {
  match_matrix_test[i,pos_max[i]] <- ifelse(probability_matrix_test[i,pos_max[i]] >= threshold_final_est,1,0)
}

household_matches_test <- data.frame()
for (i in 1:nrow(match_matrix_test)) {
  household_matches_test[i,1] <- house2016[i]
  pos_match <- which(match_matrix_test[i,] > 0)
  household_matches_test[i,2] <- ifelse(length(pos_match) == 0, NA, house2020[pos_match])
}
names(household_matches_test) <- c("Household_A","Household_B")
saveRDS(household_matches_test,"household_matches_2016_2020_threshold")

#################################
# LINEAR PROGRAMMING INDIVIDUALS - TRAINING DATA
#################################

mod <- readRDS("modIndividuals_approach3") ## Individual model file (saved before)

## INDIVIDUALS
beta_individuals <- c(coef(mod,  s = "lambda.min")[,1])

households_2014 <- unique(paired_houses$Household2014)
individuals_2014 <- data1[which(data1$NQUEST %in% households_2014),3]

train_2014 <- c(1:length(unique(data1$NQUEST)))
train_2016 <- c(1:length(unique(data2$NQUEST)))

distances_weighted_ind <-  linear_combinantion(distances_matrix_features,beta_individuals)
dist_prob_ind <- probability_matrix_ind(distances_weighted_ind,beta_individuals)

optimization_solution_train <- optimization_individuals_2(train_2014,individuals_2014,
                                                          ids1,ids2,dist_prob_ind, data1, data2,
                                                          matches_ind,household_matches_train)

saveRDS(optimization_solution_train,"IndividualModelResults_approach3_2014_2016_threshold")

#################################
# LINEAR PROGRAMMING INDIVIDUALS - TEST DATA
#################################
print("Individual Model - Test")

households_2016_test <- unique(paired_houses_test$Household2016)
individuals_2016_test <- data1_test[which(data1_test$NQUEST %in% households_2016_test),3]

test_2016 <- c(1:length(unique(data1_test$NQUEST)))
test_2020 <- c(1:length(unique(data2_test$NQUEST)))

distances_weighted_ind <-  linear_combinantion(distances_matrix_features_test,beta_individuals)
dist_prob_ind_test <- probability_matrix_ind(distances_weighted_ind,beta_individuals)


optimization_solution_test <- optimization_individuals_2(test_2016,individuals_2016_test,
                                                         ids1_test,ids2_test,dist_prob_ind_test, data1_test, data2_test,
                                                         matches_ind_test,household_matches_test)


names(optimization_solution_test) <- c("Index2016","Index2020","Correct_Match","House2020","House2016","Ind2020",
                                       "Correct","Correct_cat")

saveRDS(optimization_solution_test,"IndividualModelResults_approach3_2016_2020_threshold")

