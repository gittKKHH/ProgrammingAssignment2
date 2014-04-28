## These functions are used to create a "matrix" and then cache that matrix's inverse

## makeCacheMatrix takes a matrix in and then allows the user to either recall the matrix or set it to a new value
## after caching the Inverse, the user can get the inverse as well

makeCacheMatrix <- function(x = matrix()) {
  	m <- NULL  # Initialize m
        set <- function(y) { # Set function for resetting the values in the matrix
                x <<- as.matrix(y)
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve takes the special matrix and then cache's the inverse matrix associated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()           #query the x matrix's cache         
 	if(!is.null(m)) {           #if there is a cache
    	  message("getting cached data") 
    	  return(m)                #just return the cache, no computation needed
  	}
  	data <- x$get()             #if there's no cache
  	m <- solve(data, ...)        #we actually compute them here
  	x$setInverse(m)                #save the result back to x's cache
  	m                           #return the result

}
