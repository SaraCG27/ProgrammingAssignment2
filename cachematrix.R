# Operations such as matrix inversion can be computationally costly. 
# In this project I will first write a function to to cache the matrix's inverse,
# followed by another function to retrieve the original matrix from the cacheed result. (proofing the first one)

# Exercise 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y 
		inv <<- NULL
	}
	get <- function()  x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, 
	     get = get, 
	     setInverse = setInverse,
	     getInverse = getInverse)
}
	
#Exercise 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of "x"
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("gettng cached matrix")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
}