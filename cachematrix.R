# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {           # Initialization of the matrix to NULL
        m <- NULL                                     # Function set where the matrix will be cached
        set <- function(y) {
                 x <<- y
                 m <<- NULL
        }
        get <- function() x                           # Getting the function
        setinverse <- function(solve) m <<- solve     # Calculate the inverse
        getinverse <- function() m                    # Passes the value of the function makeCacheMatrix
        list(set = set, get = get, 
               setinverse = setinverse,
               getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                           # Retrieves the inverse
        if(!is.null(m)) {
                message("getting cached data")        # Motification if retrieved
                return(m)                             # Returnes the retrieved inverse 
        }
        data <- x$get()                               # If the matrix not retrieved -> calculate & print.
        m <- solve(data, ...)
        x$setinverse(m)
        m
}