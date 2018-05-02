###|===================|    Programming Assignment 2: Lexical Scoping       |=============|###


## makeCacheMatrix function take x as an argument, which is a square invertible matrix
## It returns a list containing four functions to be used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize the inverse property
    invrs <- NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    
    # Function the get the matrix
    get <- function() {
        # Return the matrix
        x
    }
    
    # Function to set the inverse of the matrix
    setInv <- function(inverse) {
        invrs <<- inverse
    }
    
    # Function to get the inverse of the matrix
    getInv <- function() {
        # Return the inverse property
        invrs
    }
    
    # Return a list of the methods
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve function take x as an argument, which is output of makeCacheMatrix()
## It returns inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    invrs <- x$getInv()
    
    # If the inverse has already been calculated
    if( !is.null(invrs) ) {
        # just return the inverse from the cache without calculating
        message("Getting cached data...")
        return(invrs)
    }
    
    # Otherwise, calculates the inverse
    # Get the matrix from our object
    dataX <- x$get()
    # Calculate the inverse using matrix multiplication
    invrs <- solve(dataX) %*% dataX
    # invrs = solve(dataX, ...)
    
    # Set value of the inverse in the cache with the setInv function.
    x$setInv(invrs)
    
    ## Lastly, return the matrix
    invrs
}
