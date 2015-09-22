## Calculating and caching the inverse of a matrix

## Setting the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setim <- function(invmx) im <<- invmx
  getim <- function() im
  list(set = set, get = get,
       setim = setim,
       getim = getim)

}


## Getting cached inverse matrix, if non set, calculate

cacheSolve <- function(x, ...) {
        
  im <- x$getim()
  if(!is.null(im)){
    message("getting cached inverse matrix")
    return(im)
  }
  mat <- x$get()
  im <- solve(mat, ...)
  x$setim(im)
  ## Returning a matrix that is the inverse of 'x'
  im
}
