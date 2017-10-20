## compute the inverse of a matrix.
## to save computation time, first check if the inverse exists in the cache


## create a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mat_inv <- NULL # set variable for matrix inverse
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) mat_inv <<- solve
  getinv <- function() mat_inv
  list( set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
}


## search cache for inverse to matrix. Compute inverse if not available in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("retrieving cached data")
    return(mat_inv)
  }
  
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinv(mat_inv)
  return(mat_inv)
}
