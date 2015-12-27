## Farzad Moazzami 12/27/2015
## This code will cache the inverse of a matrix to be reused 
## if needed, instead of time consuming matrix inversion at every call.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the matrix
## 2 get the matrix
## 3 set the inverse of the matrix
## 4 get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv  <<- NULL
  
  }
  get <- function() x
  setinv <- function(solve) m_inv  <<- solve
  getinv <- function() m_inv 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse matrix of the special "vector" created with the above function.
## If the inverse of the matrix is already computed. it pulls from the cache.
cacheSolve <- function(x, ...) {
  m_inv <- x$getinv()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
}

