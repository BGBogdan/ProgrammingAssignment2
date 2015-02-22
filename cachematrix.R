## Put comments here that give an overall description of what your
## functions do

## The function make CacheMatrix creates a matrix based on parameters inputted by
## the user. It then inverts it, storing the new matrix as getinverse. It then stores
## the value of the matrix, as well as that of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function()m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function checkes whether the function above already has a stored 
## inverted matrix. If it doesn't have a cahed inverse, it then proceed to 
## calculating it. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

        ## Return a matrix that is the inverse of 'x'
