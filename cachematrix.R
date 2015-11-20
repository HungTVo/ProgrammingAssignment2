## Put comments here that give an overall description of what your
## functions do
## created by: thanhhungqb@gmail.com
##

## This function make a cached matrix
## include set, get matrix and set, get for Inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(invi) inv <<- invi
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## This function use data from x (make by above function)
## if inv found then get it (cached)
## if inv not calculated, then calculate and save to x by x$setInv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
      return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    return (inv)
}
