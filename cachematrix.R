## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL

    # Function to set a new matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse since the matrix has changed
    }

    # Function to get the matrix
    get <- function() x

    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse

    # Function to get the cached inverse
    getInverse <- function() inv

    # Return a list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if the inverse is already cached

    # If inverse is already cached, return it
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }

    # Otherwise, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)  # Store the inverse in the cache

    inv  # Return the computed inverse
}

