## This 2 functions help with the computation of getting the inverse of a matrix
## If said inverse matrix exists it skips the process and gets it from the cache

## This function cache a matrix data, set its value and also gets its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## this funnction calculates the inverse of a matrix but first it checks if the matrix inverse has already calculated or if its determinant exists
## If there's no determinant it shows an error, if inverse has already been calculated it gets it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrxix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
