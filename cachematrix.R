
## This script provides a function creating a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## This function retrieves the inversed matrix. 
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("retrieve cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}