
## The first function creates a special matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function calculates the matrix created with the first function.  First it checks to see 
## if the inverse has already been calculated
## If the inverse has been calculated, a message "Getting cached data" will be displayed
## and the cached value will be used

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null (inv)){
    message("Getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
