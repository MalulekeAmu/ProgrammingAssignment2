# The functions below, namely: "makeCacheMatrix" and "cacheSolve" are arranged 
# to cache the inverse of a matrix

# function (1) "makeCacheMatrix" creates a matrix object that can cache its inverse for the input:
# invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# function (2) cacheSolve
# This function is arranged to compute the inverse of the matrix returned by
# the previous function (makeCacheMatrix).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached result")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

