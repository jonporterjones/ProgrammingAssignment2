## Returns a list of functions available on a matrix object held in memory
makeCacheMatrix <- function(x = matrix()) {    #input arg is a matrix
  inverseIt <- NULL # set the inverse to Null
  set <- function(y) {
    x <<- y  # sets x in parent environment (object)
    inverseIt <<- NULL # sets inverseIt in parent environment
  }
  get <- function() x # get the matrix.
  setInverse <- function(inverse) inverseIt <<- inverse # set the inverse of the matrix 
  getInverse <- function() inverseIt # get the inverse of the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return the inverse of an invertiable matrix
## If the inverse of the matrix is already cached in memory return it from cache.
## Otherwise calcualte the inverse, cache it, and store it.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # calls function in makeCacheMatrix
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  myMatrix <- x$get()  # get the matrix
  myInverse <- solve(myMatrix)  # calculate the inverse of the matrix.
  x$setInverse(myInverse) # set the inverse of the matrix in makeCacheMatrix
  myInverse # return the inverse I just calculated
}
