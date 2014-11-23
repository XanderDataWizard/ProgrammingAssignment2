## These functions cache the inverse of a matrix and provide the inverse of the martix
## upon request. Either from cache, if calculated and unchanged or computed and added back to cache


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                 ## "i" will hold the "inverse" and will be set to NULL every time makeCacheMatrix runs
  set <- function(y) {                      ## Set function can be used to set a new x value 
    x <<- y                                 ## Here is will load the new matrix
    i <<- NULL                              ## And reset the "inverse" to NULL
  }
  get <- function() x                       ## This function will return the orginal value for x
  setinverse <- function(solve) i <<- solve ## This will store the "inverse" under "i" using superassignment
  getinverse <- function() i                ## This will return the cached "inverse"
  list(set = set, get = get,                ## Every time makeCacheMatrix is called this list is there
       setinverse = setinverse,             ## so that calling functions know how to access these methods
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {          ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()                     ## accesses "x" and retrieves the "inverse" through getinverse() and stores it under "i"
  if(!is.null(i)) {                       ## if the "inverse" is cached this if will test true
    message("getting cached data")        ## showing the message "getting cached data" for a true "if" statement
    return(i)                             ## return the "inverse" and end the function
  }
  data <- x$get()                         ## this part will only run if the "inverse" wasn't cached, it will load the orginal x value to "data"
  i <- solve(data, ...)                   ## it will then calculate the "inverse" and store it under "i"
  x$setinverse(i)                         ## then it will store the "inverse" using "setinverse" to use superassignement for future use
  i                                       ## finally it will pring the "inverse" to screen
}