rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (outcome == "heart attack") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        columnName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")   
    }
        
    states <- levels(factor(outcomes$State))
    
    result = data.frame(hospital = c(), steate = c())
    
    for (state in states) {
        hospital <- "<NA>"
        
        forState <- outcomes[outcomes$State == state, ]
        forState[[columnName]] = suppressWarnings(as.numeric(forState[[columnName]]))
        forState <- forState[complete.cases(forState[[columnName]]), ]
        
        if (num == "best") {
            idx <- 1
        } else if (num == "worst") {
            idx <- nrow(forState)
        } else {
            idx <- num
        }
        
        if (idx <= nrow(forState)) {
            forState <- forState[order(forState[[columnName]], forState[["Hospital.Name"]]), ]    
            hospital <- forState[idx, ]$Hospital.Name
        }
        
        result = rbind(result, data.frame(hospital = hospital, state = state))
    }
    
    row.names(result) = states
    
    result
}