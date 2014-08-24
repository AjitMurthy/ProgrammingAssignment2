## makeCacheMatrix function : To create a special matrix object
## cacheSolve  function : calculates the inverse of the matrix created by function makeCacheMatrix
## Note : 
## If an inverse of the matrix cretaed by makeChaceMtrix function existis within the 
## chace(clculated previously) it will return it, and not calculate it again. 
## However if the inverse does not exits in the chache, then it will calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_mx <- NULL
  set <- function(y) {
    x <<- y
    inv_mx <<- NULL
  }
  get <- function() x
  set_inverse<- function(inverse) inv_mx <<-inverse
  get_inverse <- function() inv_mx
  list(set = set, get = get,
       setinverse = set_inverse,
       getinverse = get_inverse)
}

## cacheSolve : returns the inverse of a matrix created with makeCacheMatrix function.
# @return : if inverse is available within the Cahche, the function retrieves it else it computes
# the inverse caches and then returns the same.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mx <- x$getinverse()
  if (!is.null(inv_mx)) {
    message("getting cached inverse matrix")
    return(inv_mx)
  } 
  else {
      inv_mx <- solve(x$get())
      x$setinverse(inv_mx)
      return(inv_mx)
  }
}