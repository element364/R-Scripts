rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    forState <- outcomes[outcomes$State == state, ]
    
    if (nrow(forState) == 0) {
        stop("invalid state")
    }
    
    if (outcome == "heart attack") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")   
    }
        
    forState[[columnName]] = suppressWarnings(as.numeric(forState[[columnName]]))
    forState <- forState[complete.cases(forState[[columnName]]), ]
    
    if (num == "best") {
        idx <- 1
    } else if (num == "worst") {
        idx <- nrow(forState)
    } else {
        idx <- num
    }
    
    if (idx > nrow(forState)) {
        return (NA)
    }
    
    forState <- forState[order(forState[[columnName]], forState[["Hospital.Name"]]), ]
    forState[idx, ]$Hospital.Name
}
