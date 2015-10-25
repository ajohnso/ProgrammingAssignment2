## This series of functions allows for the calculation of the inverse of a matrix, or the
## retrieval of the cached value if it has already been calculated

## The initial function, makeCacheMatrix, establishes a special matrix object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL
      set <- function(y) {
        x <<- y
        mx <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) mx <<- inverse
      getinverse <- function() mx
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function, cacheSolve, calculates the inverse of a matrix, or retrieves the cached 
## value if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mx <- x$getinverse()
      if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
      }
      matrixdata <-x$get()
      mx <- solve(matrixdata,...)
      x$setinverse(mx)
      mx
}
