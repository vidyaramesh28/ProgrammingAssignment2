## makeCacheMatrix creates a matrix object capable of storing its inverse. 
## cacheSolve retrieves this matrix from the cache, applies solve() on it to find its inverse and prints it out to the console.

## creates a cache-able matrix

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


## finds inverse of cached matrix using solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("Getting cached data")
	return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
