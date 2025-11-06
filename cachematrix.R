## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will store the cached inverse
  
  # Set a new matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the current matrix
  get <- function() x
  
  # Set the inverse (used by cacheSolve)
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  
  # Return a list of the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get the cached inverse
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute inverse using solve()
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  # Return the newly computed inverse
  inv
}



# Create a matrix
my_matrix <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)

# Create the special matrix object
cachedMatrix <- makeCacheMatrix(my_matrix)

# First call: computes and caches the inverse
cacheSolve(cachedMatrix)

# Second call: retrieves the cached inverse
cacheSolve(cachedMatrix)
