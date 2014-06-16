## makeCacheInverse and cacheSolve together solve for the inverse of a square matrix 
## and cache the value of that computation for future use without recalculating
## the whole thing.

## makeCacheInverse takes a square matrix as input and returns a list of four 
## functions to (1) set the value of the matrix,(2) return the value of the matrix, 
## (3) set the value of the inverse of the matrix, and (4) return the value of the
## inverse of the matrix.

makeCacheInverse <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
      	x <<- y
      	i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
      	setinverse = setinverse,
            getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheInverse (above) and solves for and 
## stores the inverse of the matrix which was originally input to makeCacheInverse
## First, though, it checks to see whether it has already solved and stored that 
## value, in which case cacheSolve returns the stored solution and a message
## indicating that's what it did.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}