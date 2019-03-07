## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes 'matrix' object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	## Set sub-function to replace 'x' matrix with new 'y' matrix. usage: myCacheMatrix$set(matrix(c(1,2,3,4),2,2))
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Get sub-function to get 'x' matrix. This is original matrix, not inversed. usage: myCacheMatrix$get()
	get <- function() x

	## setInverse sub-function to set inversed matrix. usage: myCacheMatrix$setInverse(inversedMatrix)
	setInverse <- function(inverse) i <<- inverse

	## getInverse sub-function to get inversed matrix. usage: myCacheMatrix$getInverse()
	getInverse <- function() i

	
	list(set = set, get = get, setInverse = setInverse, 
		getInverse = getInverse)
}


## Write a short comment describing this function
## This function create inversed matrix and cache it. This function takes makeCacheMatrix object as an input argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()

	## If makeCacheMatrix has inversed one then do get from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

	## If no inversed matrix in cache, then get original matrix
        data <- x$get()
	
	## Begin the inversion of original matrix
        i <- solve(data, ...)

	## set inversed one back to makeCacheMatrix
        x$setInverse(i)

        i
}
