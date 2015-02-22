## The following functions are part of Assignment 2 in the Courseran R-prog course
## The two functions below are used to cache the inverse of the matrix

## The makeCacheMatrix creates a matrix than can cache its inverse. 
## The makeCacheMatrix consists of functions that:
## set/get the value of the matrix
## set/get the value of the inverse
## functions are stored in a list

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y){
        
        x <<- y
        m <<- NULL
        
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get ,setinverse = setinverse, getinverse = getinverse)

}


## The cacheSolve funcion checks if the inverse of the matrix has already been calculated
## If it was calculated in returns the inverse
## Else it computes the inverse

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
