## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix allows
#  a) matrix data to be stored and retrived
#  b) the inverse matrix to be stored ad retrieved
# It returns the list of getters and setters

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) { 
	    x <<- y 
	    inverse <<- NULL
	    return(x)
	} 
	get <- function() { 
	    return(x)
	} 
	setInverse <- function(y) { 
	    inverse <<- y 
	    return(inverse)
	}	
	getInverse <- function() { 
	    return(inverse)
	}
	lst = list(set = set, get = get,
                   setInverse = setInverse,
                   getInverse = getInverse)
        return(lst)
}


# cacheSolve gets passes the result of makeCacheMatrix
# It determines if the inverse has been computed.
# If so, it returns the result
# If not, it computes the inverse and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inverse = x$getInverse()
	if (!is.null(inverse)) { 
	  message("Getting cached data")
	  return(inverse)
	} 
	mat = x$get()
	inverse = solve(mat,...)
	x$setInverse(inverse) 
	return(x$getInverse())
}

tester <- function() { 
       x = matrix(c(1,2,2,1),2) 
       cacheX = makeCacheMatrix(x) 
       s = cacheSolve(cacheX) 
       s = cacheSolve(cacheX) 
       s = cacheSolve(cacheX) 
       return(s)
}
