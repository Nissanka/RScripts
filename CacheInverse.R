#This function creates matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#This function computes the inverse of the matrix returned by the makeCacheMatrix function. 
#If the inverse has been cached, it retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


#############
# syntax to use the above functions-like classes and methods
x <- makeCacheMatrix()
y <- matrix(c(1,0,5,2,1,6,3,5,0),3,3)

x$set(y)

x$get()

x$setinv(solve(y))
#x$setmean(mean(x$get()))
x$getinv()

cacheSolve(x)