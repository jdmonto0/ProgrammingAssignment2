## The  next function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #  inv will be our 'inverse matrix' and it's reset to NULL every time makeVector is called
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # this function returns the value of the original vector
  setinv <- function(solve) inv <<- solve # this is called by cacheSolve() during the first cacheSolve() access and it will store the value using superassignment
  getinv <- function() inv # this will return the cached value to cachemean() on subsequent accesses
  list(set = set, get = get, #  This is a list of the internal functions ('methods') so a calling function knows how to access those methods.
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache
cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
  inv <- x$getinv() # accesses the object 'x' and gets the value of the inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # we reach this code only if x$getinv() returned NULL
  inv <- solve(data, ...) # if inv was NULL then we have to calculate the inverse
  x$setinv(inv)  # store the calculated matrix in x
  inv # return the inverse of the matrix to the code that called this function
}
