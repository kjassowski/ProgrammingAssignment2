## This function creates a cacheable matrix that allows you to set the matrix and get the matrix 


makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function allows you to solve cached data
## it only solves whenever there is no cached data
cacheSolve <- function(x, ...) {
  i <- x$getinverse()     ## Return a matrix that is the inverse of 'x'
  if(!is.null(i)) {
    message("getting cached data") 
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
