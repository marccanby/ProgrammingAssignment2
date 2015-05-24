## Two functions makeCacheMatrix and cacheSolve that together compute
## the inverse of a matrix. makeCacheMatrix creates a special matrix object
## that cacheSolve uses to efficiently compute the inverse of a matrix

## The following funciton creates a special matrix object which is really a list of four functions that
## get the value of a matrix, set the value of a matrix, get the inverse of a matrix,
## and set the inverse of a matrix. Takes a matrix as input and outputs the "special matrix"
## that cacheSolve can then use to solve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The following function takes as input a special matrix object as outputted from
## makeCacheMatrix (a list of four functions) and efficiently computes the inverse of the matrix.
## First checks to see if the inverse has already been calculated (in which case it
## simply returns the inverse); if not, it computes the inverse and sets the value of the
## inverse in cache. Returns the inverse of the original matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
