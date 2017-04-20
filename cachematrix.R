## Put comments here that give an overall description of what your
## functions do

## Provides a 'data structure' type of variable that contains a list of functions
## to access and set the matrix and its inverse (if calculated elsewhere and set).

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks the 'special matrix' object passed to it to see if its inverse
## has been calculated and stored.  If not (will return NULL), it'll calculate it
## and store/set it, and return it.  If the inverse is already stored, it'll return
## it without calculating it again.

cacheSolve <- function(x, ...) 
{
  i <- x$getinv()
  if(!is.null(i)) 
  {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
