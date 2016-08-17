## Programming Assignment #2 for JHU R Programming Course (Coursera)

## This program caches the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  # Makes sure a matrix has been inputted.
  if (!is.matrix(x)) {
    stop("Input was not recognized as a matrix.")
  }
  
  inv.matrix <- NULL
  set <- function(y) {
    x <<- y
    inv.matrix <<- NULL
    
  }
  get <- function() x
  set.inverse <- function(solve) inv.matrix <<- solve
  get.inverse <- function() inv.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}

# The following creates a cache, and fetches it if available
# instead of re-running the matrix-inverse function.

cacheSolve <- function(x, ...) {
  inv.matrix <- x$get.inverse()
  
  if(!is.null(inv.matrix)) {
    message("Getting cached data")
    return(inv.matrix)
  }
  
  mat <- x$get()
  inv.matrix <- solve(mat, ...)
  x$set.inverse(inv.matrix)
  inv.matrix
  
}