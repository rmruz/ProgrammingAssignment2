## Below are two functions that are used to create a special object that 
## stores a numeric matrix and caches its inverse matrix.

## This function creates a special matrix which is really a list containing a 
## function to set its values, get them, set the values of its inverse matrix 
## and get the values of the inverse matrix 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}


## The following function calculates the inverse matrix of the special matrix created with
## the above function. However, it first checks to see if the matrix has already been
## calculated. If so, it gets the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the values of it in 
## the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
