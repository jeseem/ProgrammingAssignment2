## makeCacheMatrix acts as a cache of inverse of matrix and cacheSolve calculates
## inverse or retrieves is from cache, if it was previously calculated

## 
## This function stores the calculated inverse as a cache object (cached_inv).
## When new matrix is assigned, it resets the cached inv to Null.
## 
makeCacheMatrix <- function(x = matrix()) {
        cached_inv <- NULL
        set <- function(y) {
                x <<- y
                cached_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) cached_inv <<- inv
        getinv <- function() cached_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function first tries to retrieve a previously calculated inv
## If none exists, then it calculates new inv, stores in cache and 
## returns the calculated inv.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


## to test run following
## j <- makeCacheMatrix(matrix(c(1:4),nrow=2))
## cacheSolve(j)
##
