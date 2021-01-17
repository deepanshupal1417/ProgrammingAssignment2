##Created a function makeCacheMatrix
## It consists of 4 elements: Set, Get, SetInverse and Get Inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     ##Initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function() {x}  ##Function to Get Matrix x
  SetInverse <- function(inverse) {inv <<- inverse}
  GetInverse <- function() {inv}  ##Function to obtain inverse of matrix
  list(set = set, get = get, SetInverse = SetInverse, GetInverse = GetInverse)
  
}


##CacheSolve is used to get the cache data
CacheSolve <- function(x, ...) {
  inv <- x$GetInverse()
  if(!is.null(inv))    ##Checking if the inverse is NULL
    {
    message("Getting Cached Data")
    return(inv)    ##Returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)    ##Calculates inverse value
  x$SetInverse(inv)
  inv   ##Returns the matrix that is inverse of 'x'
}