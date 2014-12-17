
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solved) m <<- solved
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {

    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  m <- solve(data,...)
  x$setsolve(m)
  
  m
}

a<-matrix(c(-1, -2, 1, 1), 2, 2)
b<-matrix(c(-2, 2, 1, 1), 2, 2)

V <- makeCacheMatrix(a)  # need to input a matrix
cacheSolve(V)
