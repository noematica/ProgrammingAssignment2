# Matrix inversion is usually computationally expensive and there may be some benefit to caching
# the inverse of a matrix, rather than computing it repeatedly.
# The following functions use lexical scoping to store the inverse of a matrix
# in a parent environment, outside function scope. 
# Here it's the global environment, which becomes "the cache."
# If an inverse exists in the cache, the user is informed and it's returned; 
# otherwise the inverse is calculated, stored in the cache, and returned.

# makeCacheMatrix creates a special "vector," which is really a list of functions that
# set the value of the matrix,
# get the value of the matrix,
# set the value of the inverse,
# get the value of the inverse.
# set() also manages the housekeeping of removing an inverse from the cache. 
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize a NULL inverse
    inv <- NULL
    
    # Assign matrix y to matrix x in the cache;
    # reset the inverse inv to NULL in the cache.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Return the matrix x stored in the cache.
    get <- function() x
    
    # Set the inverse inv in the cache.
    setinv <- function(i) inv <<- i
    
    # Return the inverse inv from the cache.
    getinv <- function() inv
    
    # Return the list of getters and setters to be made available to other functions.
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


# cacheSolve() checks to see if an inverse is available in the cache.
# If yes, it returns this inverse. If no, it calculates the inverse of the supplied matrix,
# sets the value of the inverse in the cache, and returns the new inverse.
cacheSolve <- function(x, ...) {
    
    # Check the cache for an already-calculated inverse.
    inv <- x$getinv()
    
    # If the inverse exists, alert the user that the cached value is being retrieved;
    # then return it without calculating it again.
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # Assign the matrix stored in makeCacheMatrix() to a local variable, data.
    data <- x$get()
    
    # Calculate the inverse of the matrix stored in data.
    inv <- solve(data)
    
    # Store the calculated inverse in the cache by callng setInv() in makeCacheMatrix().
    x$setinv(inv)
    
    # Return the inverse.
    inv
}

# Sample run.
# > x <- matrix(c(1,3,3,1,4,3,1,3,4), 3, 3, byrow = TRUE) 
# > mcm <- makeCacheMatrix(x)
# > mcm$get()
# [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4

# First run:  no cache.
# > cacheSolve(mcm)
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1

# Second run: message advising that cached inverse is being returned; cached inverse returned.
# > cacheSolve(mcm)
# getting cached inverse
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1