## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The following pair of functions cache the inverse of a matrix.

## 'makeCacheMatrix' contains a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 'cacheSolve' function computes the inverse of the matrix created by 'makeCacheMatrix'.
## 1. get the inverse if calculated and return the values, 
## 2. otherwise, compute the inverse in the cache using 'setinverse' function.

cacheSolve <- function(x, ...) {
    cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
