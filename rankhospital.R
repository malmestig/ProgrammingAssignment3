## Ranking hospitals by outcome in a state

## gives the hospital with the outcome at the given index
## input: 
## state: a valid US state
## outcome: one of "heart attack", "heart failure", "pneumonia"
## num: one of "best", "worst" or numeric
## output: 
## hospital name at index
rankhospital <- function(state, outcome, num = "best") 
{
    outcome_column<-data.frame(c("heart attack", "heart failure", "pneumonia"),
                               c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
    column<-outcome_column[outcome_column[1]==outcome][2]
    if (is.na(column))
        stop("invalid outcome")
    all_outcomes <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE)
    
    state_outcomes<-all_outcomes[all_outcomes$State==state,]
    if (nrow(state_outcomes)==0)
        stop("invalid state")
    
    # strip away entries with "Not Available" in relevant outcome column 
    state_outcomes<-state_outcomes[state_outcomes[[column]] != "Not Available",]
    #convert the column to numeric
    state_outcomes[[column]]<-as.numeric(state_outcomes[[column]])
    # order by relevant outcome column and then by hospital name.
    ordered_state_outcomes<-state_outcomes[order(state_outcomes[[column]],state_outcomes[["Hospital.Name"]]),]
    
    index <- if (is.numeric(num))
        num
    else if (num=="best")
        1
    else if (num=="worst")
        nrow(ordered_state_outcomes)
    else
        stop("invalid index")
    
    ordered_state_outcomes[index,"Hospital.Name"]
}
