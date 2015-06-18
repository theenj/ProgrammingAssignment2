## The functions below calculates the inverse of a matrix and caches it for faster performance

## A function to create a special matrix which caches it inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (m) {
    x <<- m
    i <<- NULL
  }
  get <- function () x
  setinverse <- function (inverse) i <<- inverse
  getinverse <- function () i
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## A function to extract inverse of a matrix

cacheSolve <- function(x, ...) {
  m <- x$get()
  i = x$getinverse ()
  if (!is.null (i))  # Check if the inverse has been cached
  {
    message ("Getting the Cached inverse of the matrix")
    return(i)
  }
  else # Else compute the inverse and Cache it
  {
    message ("Solving the inverse and Caching it")
    inverse = solve (m)
    x$setinverse (inverse)
    return (inverse)
  }
}
