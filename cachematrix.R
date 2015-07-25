# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## reset inver for the new matrix
  inver <- NULL
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  ## get the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) inver <<- inverse
  
  ## get the inverse of the matrix
  getinverse <- function() inver
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  
  ## if the inverse has already been calculated
  if(!is.null(inver)) {
    message("getting cached data")
    ## Return cached matrix that is the inverse of 'x'
    return(inver)
  }
  
  ## else calculate the inverse 
  data <- x$get()
  inver <- solve(data)
  
  ## sets the value of the inverse in the cache 
  x$setinverse(inver)
  
  ## Return a matrix that is the inverse of 'x'  
  inver
}
