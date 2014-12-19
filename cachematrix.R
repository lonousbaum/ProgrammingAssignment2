## These two functions aim to return the inverse of a matrix, caching the inverse of a matrix rather than computing it repeatedly.
## we assume that the matrix supplied is always invertible.


## Functions creates a list setting the value of the matrix, getting the value of the matrix, setting the value of the inverse and getting the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve(x)
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function checks if the inverse of the matrix was calculated, if so, it displays the result, if not, it calculates and displays it


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}