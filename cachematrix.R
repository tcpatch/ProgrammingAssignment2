## cacheSolve works in conjunction with objects instantiated by makeCacheMatrix
## to first calculate the inverse of the matrix stored in the makeCacheMatrix
## object and cache the resulting matrix or to return the cached inverse matrix
## from the makeCacheMatrix object

## makeCacheMatrix generates an objects that holds a matrix (x), the inverted
## matrix (inverted_matrix [defaults to NULL]), and returns a list of 
## 4 functions:
##    set: resets the matrix to a new matrix and removes the cached matrix
##    get: returns the matrix (x)
##    setinverse: calculates the inverse of x and caches the value
##    getinverse: returns the inverted matrix (inverted_matrix)

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  # arguments are: x - a matrix that can be inverted
  # returns a list of functions: set, get, setinverse, getinverse
  inverted_matrix <- NULL # when instantiated, the default cached inverse is
                          # set to NULL
  set <- function(y) { # function stored in set to re-set the makeCacheMatrix 
                       # object
    x <<- y # reset the matrix to the passed value y
    inverted_matrix <<- NULL # re-NULL the cached inverted matrix
  }
  get <- function() x # function stored in get to return the matrix stored in
                      # the makeCacheMatrix object
  setinverse <- function(solve) inverted_matrix <<- solve # function stored in
                                                          # setinverse to return
                                                          # the inverse
  getinverse <- function() inverted_matrix # function stored in getinverse to
                                           # return inverted_matrix
  list(set = set, get = get, # return a list of all the functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks the makeCacheMatrix object for the stored inverted
## matrix and returns the cached value. If there is no value (i.e. NULL) 
## cacheSovle will calculate the inverse of the matrix, store it in the
## makeCacheMatrix object and return the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # This function computes the inverse of the special "matrix" returned by 
  # makeCacheMatrix above. If the inverse has already been calculated (and the 
  # matrix has not changed), then the cachesolve will retrieve the inverse 
  # from the cache.
  # arguments are: x - a makeCacheMatrix object
  # returns the inverse of x
  inverted_matrix <- x$getinverse() # calls getinverse function from the 
                                    # makeCacheMatrix objects and stores inverse
                                    # in inverted_matrix (defaults to NULL if
                                    # inverse of data has not been stored)
  if(!is.null(inverted_matrix)) { # if there is a cached value return that value
    message("getting cached data")
    return(inverted_matrix)
  }
  data <- x$get() # otherwise the data from the makeCacheMatrix object will 
                  # be stored in the data variable by calling the function get
  inverted_matrix <- solve(data) # calculate the inverse of the matrix
  x$setinverse(inverted_matrix) # set the cached value of the makeCacheMatrix
  inverted_matrix # return the inversed matrix
}