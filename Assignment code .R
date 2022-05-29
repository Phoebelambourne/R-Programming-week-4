# Week 4 assignment - final

setwd("/Users/phoebelambourne/Documents/Uni Masters/Research project/R/Github repos/R/Coursera/R-programming-week-4-")
getwd()

# Question 1 

best <- function(state, outcome) {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses =  "character")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("Invalid Outcome") 
  } else if (outcome == "heart attack") { 
    state_outcome <- outcome_data[which(outcome_data$State == state), ]
    state_outcome_best_HA <- state_outcome[which.min(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
    state_outcome_best_HA$Hospital.Name
  } else if (outcome == "heart failure") { 
    state_outcome <- outcome_data[which( outcome_data$State == state), ]
    state_outcome_best_HF <- state_outcome[which.min(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
    state_outcome_best_HF$Hospital.Name 
  } else if (outcome == "pneumonia") { 
    state_outcome <- outcome_data[which( outcome_data$State == state), ]
    state_outcome_best_PN <- state_outcome[which.min(state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
    state_outcome_best_PN$Hospital.Name
  }
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("NY", "hert attack")

# Q-s. How can I make the function output an error - "invalid state" if a state 
# which is not a value in the state column of the data frame? 

# Question 2 

rankhospital <- function(state, outcome, num) {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  bystate <- outcome_data[which(outcome_data$State == state), ]
  
  if (outcome == "heart attack") {
    bystate$ha <- bystate$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    bystate_complete <- bystate[!bystate$ha %in% "Not Available", ]
    sorted <- bystate_complete[order(as.numeric(bystate_complete$ha), bystate_complete$Hospital.Name), ]
    
  } else if (outcome == "heart failure") {
    bystate$hf <- bystate$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    bystate_complete <- bystate[!bystate$hf %in% "Not Available", ]
    sorted <- bystate_complete[order(as.numeric(bystate_complete$hf), bystate_complete$Hospital.Name), ]
    
  } else if (outcome == "pneumonia") {
    bystate$pn <- bystate$Hospital.30.Day.Readmission.Rates.from.Pneumonia
    bystate_complete <- bystate[!bystate$pn %in% "Not Available", ]
    sorted <- bystate_complete[order(as.numeric(bystate_complete$pn), bystate_complete$Hospital.Name), ]
  }
  ranked <- sorted[num, ]
  ranked$Hospital.Name
}

# Qs - how can I add "best" and "worst" as num arguments 
# How can I have NA as an output if the num is an invalid input
# How cam I make them more concise/simpl