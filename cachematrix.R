## Matrix Inversion
## Caches the inverse of a matrix to save computation time/space for
## particularly large matrices.
## Assumes the matrix is already invertible!

## Creates a special "matrix" object that can cache its inverse
## set - sets the value of the matrix
## get - gets the value of the matrix
## setinverse - sets the value of the inverse
## getinverse - gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve will retrieve the inverse from the cache.
## solve() - returns the inverse of a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
