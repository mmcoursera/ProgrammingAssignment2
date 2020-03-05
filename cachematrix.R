
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Inverse of matrix 'x'
  i <- NULL
  
  # Set matrix 'x'
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get matrix 'x'
  get <- function() x
  
  # Set inverse of matrix 'x'
  set_inverse <- function(inverse) i <<- inverse
  
  # Get inverse of matrix 'x'
  get_inverse <- function() i
  
  list(
    set = set,
    get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse
  )
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by
# 'makeCacheMatrix'. If the inverse has already been calculated (and the matrix has not
# changed), then 'cachesolve' should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  
  # Test if inverse of 'x' has already been cached
  if(!is.null(i)) {
    message("getting cached inverse of matrix 'x'")
    return(i)
  }
  
  # Calculate the inverse of matrix 'x' and set it on special matrix 'x'
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
