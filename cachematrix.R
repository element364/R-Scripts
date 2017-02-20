## Put comments here that give an overall description of what your
## functions do

## Works with matrix, can get, and set it, and can get and set inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    
    set <- function (y) {
        x <<- y
        inversed <<- NULL
    }
    
    get <- function () x
    
    setinverse <- function (inv) {
        inversed <<- inv
    }
    
    getinverse <- function () inversed
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## gets inversed matrix and caches result
cacheSolve <- function(x, ...) {
    inversed <- x$getinverse()
    
    if (!is.null(inversed)) {
        message('Getting inversed matrix')
        return (inversed)
    }
    
    data <- x$get()
    inversed <- solve(data, ...)
    x$setinverse(inversed)
    inversed
}
