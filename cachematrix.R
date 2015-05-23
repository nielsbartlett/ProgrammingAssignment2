## The first function, makeCacheMatrix creates a special "matrix", 
## which returns a list containing the following functions:
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
## 
## The second function calculates the inverse of a special matrix if it 
## hasn't already been calculated and caches the result in the cache 
## matrix and returns the inverse
##

## create a cache matrix and return the list of functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## Calculate the inverse of a cache matrix. if the inverse of a cache matrix has not yet been
## calculated ( i.e. i == NULL) then calculate it, save it and return the saved inverse i
## otherwise return the already calculated and saved inverse i.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        ## already calculated, return the saved inverse matrix
        message("getting cached data")
        return(i)
    }
    ## not yet calculated, calculate the inverse, save it and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i    
}

