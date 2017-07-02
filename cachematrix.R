## The first function, makeCacheMatrix creates a special "matrix", which contain functions to
## 	set the value of the matrix
##	get the value of the matrix
##	set the value of the inverse of matrix
##	get the value of the inverse of matrix
 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		
		s <- NULL
      	set <- function(y){
        		x <<- y
       		 	s <<- NULL
      	}
      	get <- function() x
      	setSolve <- function(inv) s <<- inv
      	getSolve <- function() s
    	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
  		if(!is.null(s)) {
    		message("getting cached data")
    		return(s)
  		}
  		data <- x$get()
  		s <- solve(data, ...)
  		x$setSolve(s)
  		s
}
