## The file contains two functions:
##
## makeCacheMatrix() - Make a cacheable matrix
## cacheSolve() - return the inverse of the cacheable matrix
##
## Example of usage
## ================
##
## > x <- matrix(1:4,2,2)
## > cx <- makeCacheMatrix(x)
## > cacheSolve(cx)  ## will calculate the inverse
## > cacheSolve(cx, TRUE)  ## will return the cached value, and a message
## > cacheSolve(cx)  ## will return the cached value, but no message
##
## > m <- matrix(c(1,3,2,-1,0,2,3,1,-1),3,3)
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)  ## will calculate the inverse
## > cacheSolve(cm)  ## will return the cached value
## > cacheSolve(cm, TRUE)  ## will return the cached value and a message
##

## Function to create a cacheable matrix
##
## Arguments
## =========
## x a matrix
##
## Returns
## =======
## a matrix that can cache the inverse
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## originally the inverse matrix will be null
    set <- function (y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This will return the inverse of a chcheMatrix
## A cacheMatrix is created by calling makeCacheMatrix()
## Arguments
## =========
## x a matrix that has been cached by calling makeCacheMatrix()
##
## Returns
## =======
## the inverse of the matrix
##
cacheSolve <- function(x, debug = FALSE, ...) {
    i<- x$getInverse()
    if(!is.null(i)) {
        if (debug) message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i ## Return a matrix that is the inverse of 'x'
}
