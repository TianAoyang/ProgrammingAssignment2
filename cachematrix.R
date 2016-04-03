## Author: Aaron Tian
## Time: 2016/04/03
## Purpose: Inplement of a special matrix that can cache it's inverse 

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m_inv <<- inv
    getinverse <- function() m_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" 
## and returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    my_inv <- x$getinverse()
    if(!is.null(my_inv)) {
        message("getting inverse")
        return(my_inv)
    }
    data <- x$get()
    my_inv <- solve(data, ...)
    x$setinverse(my_inv)
    my_inv
}
