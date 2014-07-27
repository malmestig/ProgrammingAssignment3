## Ranking hospitals in all states

## gives the hospital in each state with the outcome at the given index
## input: 
## outcome: one of "heart attack", "heart failure", "pneumonia"
## num: one of "best", "worst" or numeric
## output: 
## hospital names at index for each state
rankall <- function(outcome, num = "best") 
{
    outcome_column<-data.frame(c("heart attack", "heart failure", "pneumonia"),
                               c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
    column<-outcome_column[outcome_column[1]==outcome][2]
    if (is.na(column))
        stop("invalid outcome")
    all_outcomes <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE)
    
    index <- if (is.numeric(num))
        num
    else if (num=="best")
        1
    else if (num=="worst")
        # since worst can be a different number in each state, I assign a magic number and then look it up in the loop function
        -1
    else
        stop("invalid index")
    
    result<-vector()
    # split per state and calculate order for each
    lapply(split(all_outcomes,all_outcomes$State), function(state_outcomes) 
    {
        # strip away entries with "Not Available" in relevant outcome column 
        state_outcomes<-state_outcomes[state_outcomes[[column]] != "Not Available",]
        #convert the column to numeric
        state_outcomes[[column]]<-as.numeric(state_outcomes[[column]])
        # order by relevant outcome column and then by hospital name.
        ordered_state_outcomes<-state_outcomes[order(state_outcomes[[column]],state_outcomes[["Hospital.Name"]]),]
        if (index==-1)
            index<-nrow(ordered_state_outcomes)
        result<<-rbind(result,c(ordered_state_outcomes[index,"Hospital.Name"],ordered_state_outcomes[1,"State"]))
    })
    colnames(result)<-c("hospital","state")
    data.frame(result,row.names=NULL)
}
