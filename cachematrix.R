##
makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## following list contains functions used as input to cacheSolve()  
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse

  inv = NULL
  set = function(y) {
    # operator `<<-` is used to assign a value to an object in an environment i.e. different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## returns inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # this allows to obtain it from the cache and skips computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # use setinv function to set the value of the inverse in the cache.
  x$setinv(inv)
  
  return(inv)
}
