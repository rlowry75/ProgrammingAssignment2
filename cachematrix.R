## These functions allow you to calculate the inverse of a matrix and
## cache it for later 

## creates some language for use in the next function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  #@x needs to be a square matrix
  m <- NULL
  
  set <- function(y) {
    x <<- y  # value of x is set to y not in current environment.
    m <<- NULL # value of m is set to NULL not in current environment.
  }
  
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  
  getmatrix <- function() m
  
  list (set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)


}


## Calculates the inverse of the matrix provided. If already calcualted, uses 
## the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  
  if(!is.null(m)){
    
    message("getting cached data")
    return(m)
    
  }
  
  matrix<-x$get()
  
  m<-solve(matrix, ...)
  
  x$setmatrix(m)
  
  m
}
