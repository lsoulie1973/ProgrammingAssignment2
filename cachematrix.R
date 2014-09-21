## makeCacheMatrix allows the creation of object that is populated with
## a matrix. The cacheSolve can be used to update the makeCacheMatrix object
## to include the inverse of the matrix. This inverse is cache/saved after
## it is originaly set, thus it will not be calculated again until it set to null
## or the object is re-inititalised.
## note this caching method allows
## 1) only use the memory if the inverse values are required.
## 2) if it is required, process the inverse and saved it so it does not need to
## be recalculated.

## makeCacheMatrix allows you create an object that 
## 1) set - set/populate object with the matrix provided
## 2) get - retrieve the values of the matrix/object
## 3) setinverse - set/populate the inverse of the matrix
##     - Note this is not automatically set/run when the makeCaheMatrix object is created
## 4) getinverse - return the inverse of the matrix if it has been set else retuns NULL
makeCacheMatrix <- function(x = matrix()) {
  
  my_inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }

  setinverse <- function(inv) {
    my_inv <<- inv
  }
  
  getinverse <- function() {
    my_inv
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## CacheSolve populates the makeCacheMatrix object with the inverse of it matrix
## if it has not already be set (i.e if it is currently null). It will then return
## the inverse matrix of the provided makeCacheMatrix object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  my_inv <- x$getinverse()
  if (!is.null(my_inv)) {
    message("getting cached data")
    my_inv
  } else {
    my_inv <- solve(x$get())
    x$setinverse(my_inv)
    my_inv
  } 
}
