##  Assignment 2: Coursera Programming Assignment (Week 3 of "R-Programming") 
##  Submitted by: G. Moheyuddin (https://github.com/moheyuddin)
##  The writtwn below "makeCacheMatrix" function creates a special "matrix" object 
##  that can cache its inverse.
##  This progamming code is conatained of two functions:
##  The 1st function, given below, creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL   				## Initial Value assigned to inverse is 'NULL'
  set_matrix <- function(y) {			
    x <<- y 					## The matrix 'x' is being set
    inverse <<- NULL
  }
  get_matrix <- function() x 				## Returns the matrix 'x'
  set_inverse <- function(solve) inverse <<- solve 	## Caches the inverse 
  get_inverse <- function() inverse 			## Returns the inverse
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


##  The 2nd function, given below, computes the inverse of the special "matrix" as returned by
##  above given 'makeCacheMatrix' function. However, if the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {				## Returns an inverse matrix of 'x'
  inverse <- x$get_inverse()				## Getting the inverse
  if(!is.null(inverse)) {					## Checking for the presence of inverse
    message("getting cached data")			## Displaying the message
    return(inverse)
  }
  data <- x$get_matrix()					## Getting Matrix
  inverse <- solve(data, ...)				## Using solve() to compute inverse
  x$set_inverse(inverse)					## To cache the inverse
  inverse 						## Return a matrix that is the inverse of 'x'
}