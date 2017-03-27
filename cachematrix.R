## The two functions below are designed to accelerate a program by storing the results
## of a time-consuming computation for fast retrieval (instead of re-calculation)

## This function creates a list of functions for a matrix, to interact with a cache.
## These functions are then called upon by 'cacheSolve' to save time

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## This function checks to see if a matrix has already been inverted, else it 'solves'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
