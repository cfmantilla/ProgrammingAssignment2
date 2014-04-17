## These functions create an object that stores a matrix, calculate 
## its inverse and cache the result.
## They use the same logic as the makeVector and cachemean functions 
## in the coursera rprog-002 course examples


## This function returns a list of functions (set,get,getsolved,getsolved),
## as a side effect it updates the variable x in the parent environment 
## when functions set or setsolved are used
makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function() x
  setsolved <- function(inverse = matrix()) solved <<- inverse
  getsolved <- function() solved
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved) 
}

## This functions returns the inverse matrix of x, if the inverse 
## has been calculated before it returns its stored value, if
## there is no cached value it calculates the inverse and keeps
## the result to avoid having to recalculate
cacheSolve <- function(x, ...) {
  solved <- x$getsolved()
  if(!is.null(solved)) {
    message("getting cached data")
    return(solved)
  }
  data <- x$get()
  solved <- solve(data, ...)
  x$setsolved(solved)
  solved  
}
