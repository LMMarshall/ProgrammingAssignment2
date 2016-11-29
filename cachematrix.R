## Pair of functions to create and cache matrices and
## their inverses, then retrieve them from cache.

## Defines a square matrix, taking 
## (x=matrix(numeric vector, nrow, ncol)) as an arg and
## outputs a list to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## Check to see if the input matrix is square
    entries <- length(c(x))
    if(sqrt(entries) != sqrt(as.integer(entries))){
        message("Error: matrix is not square")
        break
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, 
         setinv=setinv, getinv=getinv)
}


## Takes a makeCacheMatrix object. Returns & stores
## that object's inverse. If inv was previously
## storied, it pulls from memory

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
