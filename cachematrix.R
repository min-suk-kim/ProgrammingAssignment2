## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # set the matrix
    # <<- assigns a value to an object in an environment different from the current one
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get the matrix
    get <- function() x
    # set the inverse
    setinv <- function(inverse) inv <<- inverse
    #get the inverse
    getinv <- function() inv
    # return a list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    # if the inverse has been calculated, get it from the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # otherwise, calculate the inverse
    mat.data <- x$get()
    m <- solve(mat.data, ...)
    # set the inverse value in the cache
    x$setinv(inv)
    inv
}
