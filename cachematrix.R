## Two functions that cache the inverse of a matrix and creates
## Function makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	 ## This is to initialize the inverse property
		inv <- NULL 
		
	## Method to set the matrix	
		set <- function(matrix) {
				m <<- matrix
				inv <<- NULL
		}
	## Method to get the matrix	
        get <- function() {
			m
		}	

	## Method to set the inverse of the matrix
		setInverse <- function(inverse) {
			inv <<- inverse
		}
	
	## Method to get the inverse of the matrix
		getInverse <- function() {
			## Return the inverse property
			inv
		}
		 ## Returns a list of the methods
        list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}	

## Function cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## This returns a matrix that is the inverse of 'x'
		m <- x$getInverse()
	
		## Return inverse if already set
        if( !is.null(m) ) {
        message("Hold on!! getting cached data")
        return(m)
}
		## Gets the matrix from our object
		data <- x$get()
		
		## Calculates the inverse using matrix multiplication
		m <- solve(data) %*% data
		
		## Sets the inverse to the object
		x$setInverse(m)
		
		## Returns the matrix
			m
}
