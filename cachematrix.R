## Functions that calculate the inverse of a matrix and cache the result

## This function stores the matrix and its inverse. Provides getters and setters functions to both values.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y){
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinv <- function(solve) inv <<- solve
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## This function calculate the inverse of the matrix if needed 
## and stores the result in the object instanciated by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}