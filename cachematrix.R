# functions cacheSolve and makeCacheMatrix are used to return the inverse of a matrix.
# If that value has already been calculated and stored in a special "matrix", than it is retrieved
# from cache, rather than being computed anew, to reduce computation

# function makeCacheMatrix takes a matrix as an argument
# and returns a list containing four functions that respectively
# 1) set the value of the matrix; 2) get the value of the matrix;
# 3) set the value of the inverse matrix; 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL # im will be the inverse of matrix x
      set <- function(y) { # this function sets the value of the matrix
            x <<- y
            im <<- NULL
      }
      get <- function() x # this function returns the matrix
      setim <- function(inversematrix) im <<- inversematrix # this function
      # sets the value of the inverse matrix
      getim <- function() im # this function returns the inverse matrix
      list(set = set, get = get, setim = setim, getim = getim) # the list of the four functions is returned
}


# function cacheSolve returns a matrix that is the inverse of x. It computes
# the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then function cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      im <- x$getim() # the value of the inverse matrix stored in x is retrieved
      if(!is.null(im)) { # if that value is not NULL, i.e. if the inverse matrix
            # has already been calculated...
            message("getting cached data") # ...then that value is returned and the user
            return(im)              # is told that the value is being retrieved from the cache
      }
      data <- x$get() # otherwise, the value of the matrix is retrieved...
      im <- solve(data, ...) # ...so that the inverse matrix can be computed
      x$setim(im) # then the computed value is stored in x
      im # and it is returned
}