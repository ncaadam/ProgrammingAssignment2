## These two function combined will allow you to create a special matrix, store its value, and get that value if needed

## this function creates a special matrix
## it allows you to get and set the following: matrix's value, inverse's value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## the cacheSolve function calculates the inverse of a special vector
## it checks if the inverse has been calculated or not first
## if it has been calculated, it skips this computation

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}