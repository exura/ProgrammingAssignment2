## Creates a special matrix object that can store the inverse,
## to potentially save time.

makeCacheMatrix <- function(x = matrix()) {

	  ## creates an object which can store the inverse of the matrix x
	  ## which is created by setinv() and retrieved by getinv()
	  inv <- NULL

	  ## creates a function to set another matrix in the
	  ## special matrix object (and then the inv must be nulled)								
        set <- function(y = matrix()) {					
                x <<- y						
                inv <<- NULL
        }

	  ## Function to retrieve the actual matrix
        get <- function() x

	  ## Function to set the inverse of the matrix
        setinv <- function(inverse) inv <<- inverse

	  ## Function to retrieve the inverse of the matrix
        getinv <- function() inv

	  ## Outputting the object with the functions defined
	  ## and the environment
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns the inverse to the special matrix object defined in
## makeCacheMatrix (returns the cached inverse if available)

cacheSolve <- function(x, ...) {

        ## Retrieve the current inverse of the special
	  ## matrix object defined in makeCacheMatrix
	  inv <- x$getinv()

	  ## If the matrix inverse is already calculated for
	  ## the object the inverse is returned
        if(!is.null(inv)) {
                message("Inverse retrieved from cache!")
                return(inv)
        }

        ## Otherwise the actual matrix is retrieved,	
        data <- x$get()

	  ## the matrix inverse is calculated,
        inv <- solve(data, ...)

	  ## the inverse is appended to the special object,
        x$setinv(inv)

	  ## and returned!
        inv
}
