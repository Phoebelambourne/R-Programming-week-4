# Q3 draft


library(dplyr)

# 2 arguments - outcome and hospital rank (num)

rankall <- function(outcome, num){ 
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses =  "character")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("Invalid Outcome") 
    
  } else if (outcome == "heart attack") {
    outcome_data$ha <- outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    complete <- outcome_data[!outcome_data$ha %in% "Not Available", ]
    ranked <- complete[order(as.numeric(complete$ha), complete$Hospital.Name), ]
    
  } else if (outcome == "heart failure") {
    outcome_data$hf <- outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    complete <- outcome_data[!outcome_data$hf %in% "Not Available", ]
    ranked <- complete[order(as.numeric(complete$hf), complete$Hospital.Name), ]
    
  } else if (outcome == "pneumonia") {
    outcome_data$pn <- outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    complete <- outcome_data[!outcome_data$pn %in% "Not Available", ]
    ranked <- complete[order(as.numeric(complete$pn), complete$Hospital.Name), ]
  }
  ranked %>% 
    group_by(State) %>%
    summarise_all(nth, num) %>%
    select(Hospital.Name, State)
  
} 

tail(rankall("heart attack", 20)) 

# This works with certain arguments but not others 
# ie doesnt work with this call: 
tail(rankall("heart failure"), 10)
# Error - argument "num" missing - not sure what to set as the "num" default

# Not sure how to take "best" or "worst" as num arguments 



# if outcome = heart attack 
outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses =  "character")
outcome_data$ha <- outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
hacomplete <- outcome_data[!outcome_data$ha %in% "Not Available", ] # gets rid of NAs 
rankedha <- hacomplete[order(as.numeric(hacomplete$ha)), ]
rankedha %>% 
  group_by(State) %>%
  summarise_all(nth, 30) %>%
  select(Hospital.Name, State)
