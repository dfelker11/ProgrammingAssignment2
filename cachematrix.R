## This code creates a function to calculate and cache the inverses of a matrix

## Creates a special matrix objset that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # initialize
    set <- function(y) { # new function where value is cached
        x <<- y
        inv <<- NULL
    }
    get <- function() x # get value of inverse
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv # pass the value of the function
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, and has not changed, then it 
## retrieves it from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    # if the inverse exists, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if the inverse does not exist, calculat it and return it
    mat.data <- x$get()
    inv <- solve(mat.data,...)
    x$setinverse(inv)
    return(inv)
}
