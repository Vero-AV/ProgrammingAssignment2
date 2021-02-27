## Caching the Inverse of a Matrix

## The function makeCacheMatrix creates a list containing a function to 
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the inverse of the matrix
    ## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list( set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## The function cacheSolve outputs the inverse of special "matrix" 
## returned by makeCacheMatrix by checking if the inverse has already been cached
    ## if so, return the cached inverse
    ## otherwise, calculate the inverse, cache it and return it



cacheSolve <- function(x, ...) {
       inv <-x$getinverse()
       if(!is.null(inv)){
         message("getting cached inverse matrix")
         return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
}
