## For Programming Assignment 2
## I have rewrite makeVector and cachemean function for matrix-needs. 
## It looks as most reasonable variant ^^


## This function creates a special "matrix" object that can cache its inverse.
## This function contain a list of a function:
    # 1) set the value of the matrix
    # 2) get the value of the matrix
    # 3) set the value of the inversed matrix
    # 4) get the value of the inversed matrix
makeCacheMatrix <- function(in_matrix = matrix()) {
    inversed <- NULL
    set <- function(set_matrix){
        in_matrix <<- set_matrix
        inversed <<- NULL
    }
    get <- function() in_matrix
    setinverse <- function(inv) inversed <<- inv
    getinverse <- function() inversed
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

## !!! This function assumes that the matrix is always invertible.
cacheSolve <- function(cache_matrix, ...) {
     inversed <- cache_matrix$getinverse()
     if(!is.null(inversed)){
         message("getting cached data")
         ## Return a matrix that is the inverse of 'cache_matrix'
         return(inversed)
     }
     data <- cache_matrix$get()
     inversed <- solve(data, ...)
     cache_matrix$setinverse(inversed)
     # return inversed matrix
     inversed
}