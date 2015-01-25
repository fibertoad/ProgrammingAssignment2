## These two functions are to create then store the inverse
## of a matrix in cache memory.

## This first function create a matrix, then gets the inverse 
## of such matrix and stores it in cache memory insides its 
## environment

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  mat<- function(u) {
    x<<- u
    i<<- NULL
  }
  get<- function() x
  setinverse<- function(solve) i<<- solve
  getinverse<- function() i
  list(mat=mat, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This second function checks to see if the matrix as
## been stored in cache memory, if it is not in cache 
## it solves for the inverse and puts it in cache.

cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  data<- x$get()
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
        ## Return a matrix that is the inverse of 'x'

