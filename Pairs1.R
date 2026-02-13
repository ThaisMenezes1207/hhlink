###########
# FUNCTION
###########

source("Basic variables.R")

############################################
# PAIRS DATA 2014-2016
############################################

###########
# DATA - 2014 and 2016
###########

#setwd("~/Desktop/UCD/Record Linkage/Data/Survey of Household Income and Wealth - Italy/Files")
data <- readRDS("Data Italy - Final")

matches <- readRDS("Matches")
matches_ind <- readRDS("Matches Ind")

data1 <- data[which(data$Year == 2014),]
data2 <- data[which(data$Year == 2016),]

###########
# BASIC VARIABLES
###########
basic <- basic_variables(data,years = c(2014,2016))

range2014_ind <- basic$range1
range2016_ind <- basic$range2

range2014_h <- basic$range_h1
range2016_h <- basic$range_h2

id <- basic$id

n <- nrow(data[data$Year == 2014,])
ids1 <- id[1:8156,]
ids2 <- id[c(8157:nrow(id)),]
for(j in 1:nrow(ids2)){
  range2 <- unlist(ids2$Index[j])-n
  ids2$Index[[j]] <- I(range2)
}

household2014_id <- NULL
pos2014 <- NULL
for(i in 1:nrow(ids1)){
  a <- rep(ids1$Household[i],length(unlist(ids1$Index[i])))
  b <- unlist(ids1$Index[i])
  household2014_id <- c(household2014_id,a)
  pos2014 <- c(pos2014,b)
}

household2016_id <- NULL
for(i in 1:nrow(ids2)){
  a <- rep(ids2$Household[i],length(unlist(ids2$Index[i])))
  household2016_id <- c(household2016_id,a)
}


optimization_problem <- readRDS("IndividualModelResults_2014_2016")

pairs_data <- data.frame(Index2014 = as.numeric(),
                         Index2016 = as.numeric(),
                         Ind2016 = as.numeric(),
                         Pair_matched = as.numeric(),
                         True_match = as.numeric(),
                         Correct = as.numeric(),
                         Correct_pair = as.numeric(),
                         Ind2016_c = as.numeric()
)

range2016 <- (range2016_ind - 19366)
inds_2016 <- data2$IND

for(i in range2014_ind){
  print(i)
  
  i_range = i
  j_range = range2016
  
  vec_2014 <- NULL
  vec_2016 <- NULL
  vec_match <- NULL
  vec_label <- NULL
  vec_true <- NULL
  vec_2016_c <- NULL
  
  rep1 <- rep(i_range,length(j_range))
  rep2 <- rep(0,length(j_range))
  rep3 <- rep(NA,length(j_range))
  true_match <- rep3
    
  vec_2014 <- c(vec_2014,rep1)
  vec_2016 <- c(vec_2016,j_range)
  vec_2016_c <- c(vec_2016_c,inds_2016[j_range])
    
  pos <- which(optimization_problem$Index2014 == i)
  match_found <- optimization_problem$Index2016[pos]
  
  if(!is.na(match_found)){
    
  
  pos2 <- which(j_range == match_found)
  rep2[pos2] <- 1
  label2016 <- inds_2016[j_range[pos2]]
  
  rep3[pos2] <- label2016
  }
    
  vec_match <- c(vec_match,rep2)
  vec_label <- c(vec_label,rep3)
    
  true <- optimization_problem$Correct_Match[pos]
  rep4 <- rep(true,length(j_range))
  vec_true <- c(vec_true,rep4)
  
  beg <- nrow(pairs_data) + 1
  end <- nrow(pairs_data) + length(vec_2014)
  pairs_data[beg:end,1] <- vec_2014
  pairs_data[beg:end,2] <- vec_2016
  pairs_data[beg:end,3] <- vec_label
  pairs_data[beg:end,4] <- vec_match
  pairs_data[beg:end,5] <- vec_true
  pairs_data[beg:end,8] <- vec_2016_c
}

for (i in 1:nrow(pairs_data)) {
  print(i)
  true_match <- pairs_data$True_match[i]
  match_found <- pairs_data$Ind2016[i]
  true_match_t <- ifelse(pairs_data$Ind2016_c[i] == true_match ,true_match,NA)
  
  if(pairs_data$Pair_matched[i] == 1){
    if(is.na(true_match)){
      pairs_data$Correct[i] <- 0
    } else {
      pairs_data$Correct[i] <- ifelse(match_found==true_match,1,0)
    }
  } else if(!is.na(true_match_t)){
    pairs_data$Correct[i] <- 0
  } else if(is.na(true_match_t)){
    pairs_data$Correct[i] <- ifelse(is.na(match_found),1,0)
  }
}

pairs_data$Correct_pair <- ifelse(pairs_data$True_match == pairs_data$Ind2016_c,"M","NM")
pairs_data$Correct_pair <- ifelse(is.na(pairs_data$True_match),"NM",pairs_data$Correct_pair)

saveRDS(pairs_data,"PairsData_2014_2016")
