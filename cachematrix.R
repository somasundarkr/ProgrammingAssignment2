## Matrix inversion is a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Objective assignment  is to write a pair of functions that cache the inverse of a matrix.
## Objectives for implementation
## makeCacheMatrix This function creates a special "matrix" object that can cache its universe.
## cacheSolve: This function computes the inverse of the special "matrix" returned # # by makeCacheMatrix above. If the inverse has already been calculated (the matrix # has not changed), then the cacheSolve should retrieve the invers from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R.

## Write a short comment describing this function

## Objective 1: makeCacheMatrix Creation

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL                                 ## Null is assigned the varible inverse which receives the matrix
    set <- function(y) {                            ## function receives the value
        x <<- y                                     ## updation of matrix with new value
        inverse <<- NULL                            ## reset the new with inverse
    }  
    get <- function() x                              ## Receives actual input value
    setinverse <- function(solve) inverse <<- solve  ## set the inverse value of the matrix
    getinverse <- function() inverse                 ## Get the inverse value of the matrix
    list(set = set, get = get,                       
         setinverse = setinverse,
         getinverse = getinverse)
}

## objective 2: cacheSolve function creation

cacheSolve <- function(x, ...) {
    ## Return a matrix which is the inverse of 'x'
    inverse <- x$getinverse()                 # Get the inverse of the matrix
    if(!is.null(inverse)) {                   # check if its inverse of the matrix has been calculated
        message("getting cached data")        # if done, prints message as "getting cached data" and
        return(inverse)                       # returns the inverse of the matrix and skips the computation.
    }
    #If not the inverse matrix has been calculated:
    data <- x$get()                           # get the actual matrix       
    inverse <- solve(data, ...)               # calculating the inverse of the matrix using solve
    x$setinverse(inverse)                     # updating the variable that holds the inverse of the matrix
    inverse                                       
}
