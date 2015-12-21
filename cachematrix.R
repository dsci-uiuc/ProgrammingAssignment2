## Functions designed to calculate the inverse of a matrix, caching any repeated computations.



makeCacheMatrix <- function(x = matrix()) {
    # Creates a structure to be used in the cacheSolve() function that will allow caching of computation results.
    #
    # Args:
    #   x: The matrix that will have its computation results stored.
    # 
    # ReturnS:
    #   A list that contains functions used for getting and setting the cached matrix and its inverse.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
    # Computes the inverse of a matrix (caching results for repeated computations), that is contained by 'x'
    #
    # Args:
    #   x: A list created by makeCacheMatrix() that contains functions to 
    #       get and set a cached matrix and its inverse
    #
    # Returns:
    #   A matrix that is the inverse of 'x'
    
    # check if the calculated inverse is already available for this matrix
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if it doesn't already exist, calculate the inverse of the matrix, 
    #   and store it before returning the reuslt to the caller
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
