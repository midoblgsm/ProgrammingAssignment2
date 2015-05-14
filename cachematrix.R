## The following code allows to create the inverse of a matrix
## The inversed matrix is saved in the cache for the first creation
## It is used afterwards instead of un-needed processing


## This method creates the inverse of the matrix and sets up the needed functions to get the cached value of the inverse 

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x

  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function gets the inverse of a matrix
## It verifies if there is any cached value for the inverse
## If the cached inverse exists it is returned 
## Otherwise it calls the makeCacheMatrix to inverse the matrix and cache it for further usages
## The inversed matrix is returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
