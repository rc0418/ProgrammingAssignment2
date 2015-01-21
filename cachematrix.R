##Often, when using long vectors or using multiple loops, functions can take a long time to execute.
##The purpose of these functions are to save computing time by caching known values. Therefore, when the next part of the 
##function is executed and the needed value is the same as the stored value, cached value is used, instead of being recomputed.
##These funcitons also compare the required value to the cached value, recomputing the value if they are not the same. 

##makeCacheMatrix is a function allowing you to create and set a matrix as well as getting and setting its inverse 
# - stored as variable 'm'. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
##cacheSolve returns the inverted matrix. If the inverse matrix is already available, it will be returned without the need of 
##computation. If the inverse matrix is not found, it is computed and then returned.  
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
