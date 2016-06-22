## These two functions are used to create an object that stores a matrix
## and it's inverse.

## The makeCacheMatrix function creates an object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    iMatrix <- NULL
    set <- function(y) {
        x <<- y
        iMatrix <<- NULL
    }
    get <- function() x
    setInv <- function(inv) iMatrix <<- inv
    getInv <- function() iMatrix
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The cacheSolve function computes the inverse of the object returned by the function
## makeCacheMatrix. If the inverse has been calculated, the function would retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    iMatrix <- x$getInv()
    if(!is.null(iMatrix)) {
        message("getting cached data")
        return(iMatrix)
    }
    data <- x$get()
    iMatrix <- solve(data, ...)
    x$setInv(iMatrix)
    iMatrix
}