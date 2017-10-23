## Put comments here that give an overall description of what your functions do:

##1 makeCacheMatrix: creates an object containing the following lists:
      ## 1. set: Setting the value of a matrix.
      ## 2. get: bring back values of the matrix.
      ## 3. setinverted: sets the values for solved inverse matrix (This only stores the value without computing).
      ## 4. getinverse: get the inverse value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      ## Generate a null inverse variable
      InvVar <- NULL
      ## Function set values.
            # use y as argument and reset inverse variable.
      set <- function(y) {
            x <<- y
            InvVar <<- NULL
      }
      ## Return variable defined by set function.
      get <- function() x
      ## Set the inverse matrix values by storing without computing.
      setinverted <- function(solve) InvVar <<- solve
      # get the value of the inverted matrix.
      getinverted <- function() InvVar
      # get a list of the above functions using R objects.
      list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}

##2 cacheSolve: Either retrives the cached solution of:
            ## 1. An inverse matrix or solves, stores, and retrieve.
            ## 2. Makes and caches an inverted version of the original matrix.
            ## 3. Creates a function that retrives the cached inverted matrix from its argument
            ##      or computes and stores the value for later use.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x' and set the inverted value to a cached value.
      InvVar <- x$getinverted()
      ## checks for existence, if so - returns value.
      if(!is.null(InvVar)) {
            message("retrieving cached data")
            return(InvVar)
      }
      ## If the above does not work it retrives the matrix value from the object.
      data <- x$get()
      ## Calculates the inverse matrix and assigns to the "InvVar" variable.
      InvVar <- solve(data, ...)
      ## Caches the solves matrix in the makecacheMatrix R object.
      x$setinverted(InvVar)
      ## Get the final value.
      InvVar
}

## Uncomment below for testing the code
##matrixtest <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
##cacheSolve(makeCacheMatrix(matrixtest))