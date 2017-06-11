## This file is a fork of the repository for programming assignment 2.

######################################################################################################
## PS: My submission reuses the code and comment in the given example from the assignment
######################################################################################################

# creates a special "matrix", which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
#
# PS: My submission reuses the code and comment in the given example from the assignment
# 
makeCacheMatrix <- function(x = matrix()) {
  # variable that holds the inverse matrix
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    
    # the matrix is updated, resetting the inverse
    inverse <<- NULL 
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
# it calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function
# 
# PS: This submission reuses the code & comment in the given example from the assignment

cacheSolve <- function(x, ...) {
  # step 1: Get the inverse
  inv <- x$getinv()
  
  # step 2: if it is calculated before i.e. not null, return the old value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # step 3: hmm.. inverse need to be calculated, get the matrix
  data <- x$get()
  
  # step 4: Calculate the inverse
  inv <- solve(data)
  
  # step 5: Cache it
  x$setinv(inv)
  
  # Step 6: Return the inverse
  inv
}
