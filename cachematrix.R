## Put comments here that give an overall description of what your
## functions do

## set and get the value of the matrix and its inverse, create cache values

makeCacheMatrix <- function(x = matrix()) {
	 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## if the inverse has already been calculated then call it from cache, otherwise calculate and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
