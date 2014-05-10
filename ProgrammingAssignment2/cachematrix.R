## There are 2 functions in this R file.
## 1. makeCacheMatrix : This function creates a special "matrix" object that can cache
##                      its inverse.
## 2. cacheSolve      : This function computes the inverse of the special "matrix" returned 
##                      by `makeCacheMatrix` above. If the inverse has already been 
##                      calculated (and the matrix has not changed), then the 
##                      this function will retrieve the inverse from the cache.

## Assumptions :- The matrix for which inverse is to be calculated is
##                a) A square matrix
##                b) An invertible matrix
##

## Name of Function :- makeCacheMatrix
## Input :- The matrix for which inverse is to be calculated
## Output :- A special matrix(list) which can be used to retrieve the cached inverse
makeCacheMatrix <- function(x = matrix()) {

        ## imCache is a list(special matrix) which will be used to cache the inverse matrix
        imCache <- NULL
        
        ## set is used to save the matrix for which we have to calculate the inverse
        set <- function(y) {
                x <<- y
                imCache <<- NULL
        }
        
        ## get will retrieve the matrix for which we have to calculate the inverse
        get <- function() x
        
        ## setinv is used to save the inverse matrix
        setinv <- function(inv) imCache <<- inv
        
        ## getinv will retrieve the inverse matrix
        getinv <- function() imCache
        
        ## imCache will be made into a list of functions, using this list one can use
        ## the "set", "get", "setinv", "getinv" functions to cache and retrieve 
        ## the inverse matrices
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Name of Function :- cacheSolve
## Input :- The special matrix created using makeCacheMatrix
## Output :- Inverse of the matrix
cacheSolve <- function(x, ...) {

        ## We first check if the inverse of the matrix is cached
        invMatrix <- x$getinv()
        
        ## If yes, we return the cached value
        if(!is.null(invMatrix)) {
                message("Getting cached data")
                return(invMatrix)
        }
        
        ## If no, we find the inverse of the matrix
        data <- x$get()
        invMatrix <- solve(data, ...)
        
        ## ..  and cache the result
        x$setinv(invMatrix)
        invMatrix
}
