## Calculate the inverse of a matrix, and cache it.
## Return the inverse of the matrix by first checking if it is present
## as cache data (in which case it is super quick),
## if not, then by recalculating it.

## This function takes as input a matrix and returns a list of functions 
## which can get and set the value of the matrix and that of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 ##Set inverse of matrix to be NULL
  
  set <- function(y) {                      ##If the matrix is not present in cache,   
    x <<- y                                 ##then assign its value, and
    m <<- NULL                              ##set the inverse to be NULL.
  }
  
  get <- function() x                       ##Return value of matrix
  
  setinv <- function(inv) m <<- inv         ##If the inv is not in cache,
                                            ##assign the new inv.
  getinv <- function() m                    ##Return the inverse of the matrix.
  
  list(set = set, get = get,                ##Return the four functions
       setinv = setinv,                     ##defined above.
       getinv = getinv)
}


## Take as input a list of functions which set and get values of a matrix
## and of its inverse; and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()                    ##Get inverse of matrix.
  
  if(!is.null(m)) {                  ##If value of inv is not NULL,
    message("getting cached data")   ##then it is not a new matrix,
    return(m)                        ##so return the inv from cache.
  }
  
  data <- x$get()                   ##Otherwise, get the value of matrix,
  m <- solve(data, ...)             ##calculate the inverse,
  x$setinv(m)                       ##store the inverse in cache,
  m                                 ##and return the inverse.
}
