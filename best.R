best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    forState = outcomes[outcomes$State == state, ]
    
    if (nrow(forState) == 0) {
        stop("invalid state")
    }
    
    if (outcome == "heart attack") {
        columnName = "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        columnName = "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        columnName = "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")   
    }
    
    forState[[columnName]] = suppressWarnings(as.numeric(forState[[columnName]]))
    forState <- forState[complete.cases(forState[[columnName]]), ]
    forState[which.min(forState[[columnName]]), ]$Hospital.Name
}