## Create a list of functions for to cache inverse 
  #matrices
  	
makeCacheMatrix <- function(x = matrix()) {

    #set the value of the matrix
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    #get the value of the matrix
    get <- function() x
    
    #set the values of the inverse matrix
    setinverse <- function(solve) m <<- solve
    
    #get the values of the inverse matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
