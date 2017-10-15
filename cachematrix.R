## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a "matrix", calculate its inverse and cache it. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get < function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
##This function calculate the inverse of the "matrix" stored by makeCacheMatrix.
##If the inverse has already calculated, then it only retrieve it from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
