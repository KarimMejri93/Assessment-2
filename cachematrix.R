## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can be chached and laoded from chache memory without recomputing it
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) { ##SET THE MATRIX Y TO X
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(NInverse) inverse <<- NInverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function load the matrix if it is already in cache or recompute it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
