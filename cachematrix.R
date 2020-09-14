## makeCacheMatrix will create and cache the inverse matrix
## as per the assignment required

## get the list of the cache 

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL
  set <- function(y) {
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x
  setInvMat <- function(inverse){ InvMat <<- inverse}
  getInvMat <- function(){InvMat} 
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)

}


## cache solve if the the cache not got 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInvMat()
  if(!is.null(InvMat)) {
    message("getting cached data")
    return(InvMat)
  }
  data <- x$get()
  InvMat <- solve(data, ...)
  x$setInvMat(InvMat)
  InvMat
  
}
