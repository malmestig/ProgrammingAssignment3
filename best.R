## Finding the best hospital in a state

## calculates the hospital with the lowest mortality rate
## input:
## state: a valid US state
## outcome: one of "heart attack", "heart failure", "pneumonia"
## output:
## the name of the best hospital
best <- function(state, outcome) 
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
    # strip away entries with "Not Available" in relevant column 
    state_outcomes<-state_outcomes[state_outcomes[[column]] != "Not Available",]
    #convert the column to numeric
    state_outcomes[[column]]<-as.numeric(state_outcomes[[column]])
    # take the minimum of the relevant column
    minimum<-min(state_outcomes[[column]])
    # entries with the minimum value in the column.
    names<-state_outcomes[state_outcomes[[column]] == minimum,"Hospital.Name"]
    # return the alphanumerically smallest (even if only one...)
    sort(names)[1]
}
