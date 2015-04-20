## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	# Set the value of the matrix `x'
	set <- function(y) {
		inv <<- NULL
		x   <<- y
		
	}

	# Get the value of `x'
	get <- function() x

	# Set the value of `inv', the inverse of `x'
	setinv <- function(i) inv <<- i

	# Get the value of `inv'
	getinv <- function() inv

	list(
		set = set, 
		get = get,
		setinv = setinv,
		getinv = getinv
	)
}


## Computes the inverse of the special n-by-n "matrix" `x' returned by `makeCacheMatrix' above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve' retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("Retrieving cached data")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setinv(inv)
	inv
}
