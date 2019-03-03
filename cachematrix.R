## Put comments here that give an overall description of what your
## functions do

## This function is creating a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(p) {
    l <<- p
    inver <<- NULL
  }
  get <- function() l
  setInver <- function(inverse) inver <<- inverse
  getInver <- function() inver
  list(set = set,
       get = get,
       setInver = setInver,
       getInver = getInver)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(r, ...) {
  ## Return a matrix that is the inverse of r
  inver <- x$getInver()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  a <- x$get()
  inver <- solve(a, ...)
  x$setInver(inver)
  inver
}
