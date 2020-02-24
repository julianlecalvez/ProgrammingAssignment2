## Here is a set of function that helps caching an inverted matrix.
## It works with a CacheMatrix function which returns a list of function to set and set the matrix and its inverse 
## Then the cacheSolve function is used to compute the inverse if the CacheMatrix object in input didn't already store the inverted matrix 

## makeCacheMatrix takes a matrix as an input, which is stored in the "x" variable 
## the function exposes 4 functions : 
## ==> get() and set() to read and write the input matrix
## ==> getsolve() and setsolve() to read and write the inverted matrix in the "m" variable

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


## cacheSolve is the function that helps to compute the inverted matrix ONLY IF it wasn't already computed. 
## the function takes the cacheMatrix object from the above function. 
## then it checks the if an inverted matrix has already been computed (aka. if getsolve() returns NULL)
## if not, it computes the inverted matrix and store it. 
## finally, the function returns the inverted matrix from the cacheMatrix object. 

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


