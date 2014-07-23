## the first function, makeCacheMatrix will create
## a "special" vector functions that will allow the
## inverse of a function to be stored in cache

## this function takes in a matrix and makes a 
## vector of functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setcache <- function(solve) inv <<- solve
        getcache <- function() inv
       list(get = get, set = set,
            setcache =setcache, 
            getcache = getcache)

}


## This function will store the inverse of the matrix in
## the cahce from  the "special" vector if it has not
## been stored. If it has, it will call the inverse from
## the cache

cacheSolve <- function(x, ...) {
        inv <- x$getcache()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setcache(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
