## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## The first function, 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse. 
## This object is a list containing a function to
## This function uses the '<<-' operator to assign values to the 'free' variables 
makeCacheMatrix <- function(x = matrix()) {
		## Create a special "matrix" object
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invrs <<- solve
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function 'cacheSolve' computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. 
## The function first checks to see if the inverse has already been calculated.
## If so, then it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse of a matrix in the cache via the `setinverse`
## For this assignment we assume that the matrix supplied is always invertible function.	
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinverse(invrs)
        invrs
}
