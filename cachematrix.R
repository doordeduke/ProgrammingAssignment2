## These functions cache the inverse of the matrix and retrieve the cached inversion / perform the 
## inversion and return it

## Referenced Bill Hilton's "Making Sense of Assignment 2" thread available at
## https://class.coursera.org/rprog-009/forum/thread?thread_id=457 as well as 
## the instructions / example for this assignment

## This function caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { ## the input x will be a matrix
      inv <- NULL ## inv is the inverse of our matrix and is reset whenever makeCacheMatrix is called
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x ## calls the input matrix
      setinv <- function(solve) inv <<- solve ## called by cacheSolve when it is run the first time 
                                          ## value of inv set (cached) using superassignment
      getinv <- function() inv ## cached value of inv returned to cacheSolve
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function returns the inverse of the matrix either by retrieving it from the cache (if available)
## or solving for it (if not)

cacheSolve <- function(x, ...) { ## x is created by makeCacheMatrix function
      inv <- x$getinv() ## the inverse of x is retrieved 
      if(!is.null(inv)) { ## if the value isn't NULL
            message("Getting cache data") 
            return(inv) ## return the cached value
      }
      data <- x$get() # if the inverse of x is NULL
      inv <- solve(data, ...) # calculate the inversion
      x$setinv(inv) # store the value of inv in makeCacheMatrix
      inv # return the value

}
