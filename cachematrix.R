
## Create a cache marix object that can be used to calculate the inverse once.
##
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Set the matrix being cached.
##  M <- cacheMatrix$get()  # Return the matrix being cached.
##  cacheMatrix$setInverse(solve(data, ...)) # Contain cached inverse of x
##  cacheMatrix$getInverse()                 # Get the cached inverse of x


## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
     invFunc <- x$getInverse()  # Return a matrix which is the inverse of 'x'
     if(!is.null(invFunc)) {
             message("getting cached data")
             return(invFunc)
     }
     data <- x$get()
     invFunc <- solve(data, ...)
     x$setInverse(invFunc)
     invFunc
}
