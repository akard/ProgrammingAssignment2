## This pair of functions cache the inverse of a matrix.


## makeCacheMatrix defines a list of functions to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)

}


## cacheSolve retrieves the matrix inverse from the cache if the matrix inverse has already been calculated. 
## If the inverse is not in the cache, this function computes the inverse of the matrix and sets the inverse in the cache using the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
