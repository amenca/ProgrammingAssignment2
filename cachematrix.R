## Calculating and caching the inverse of a matrix

## Setting the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){ ##set matrix given
    x <<- y
    ##when new matrix is set, reset inverse matrix to be calculated again
    im <<- NULL
  }
  get <- function() x ##Return the matriz
  setim <- function(invmx) im <<- invmx ##Store the inverse
  getim <- function() im ##Return the inverse
  list(set = set, get = get,
       setim = setim,
       getim = getim)

}


## Getting cached inverse matrix, if non set, calculate
cacheSolve <- function(x, ...) {
  im <- x$getim()
  ## If the inverse matrix is returned don't calculate it
  if(!is.null(im)){
    message("getting cached inverse matrix")
    return(im)
  }
  ## If inverse matrix is NULL...
  ##Get the matrix
  mat <- x$get()
  ##Calculate inverse matrix
  im <- solve(mat, ...)
  ##Set the inverse matrix
  x$setim(im)
  ## Return the inverse matrix of 'x'
  im
}
