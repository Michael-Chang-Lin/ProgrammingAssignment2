## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL  # Step 1: Initialize the inverse as NULL (cache is empty)
      
      set <- function(y) {
            x <<- y   # Step 2: Store the new matrix
            inv <<- NULL  # Reset the cached inverse since the matrix has changed
      }
      
      get <- function() x  # Step 3: Retrieve the stored matrix
      
      setInverse <- function(inverse) inv <<- inverse  # Step 4: Store the computed inverse in cache
      
      getInverse <- function() inv  # Step 5: Retrieve the cached inverse
      
      # Step 6: Return a list of the four functions
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()  # Step 1: Check if the inverse is already cached
      
      if (!is.null(inv)) {  # Step 2: If inverse exists, return cached result
            message("getting cached data")
            return(inv)
      }
      
      mat <- x$get()  # Step 3: Get the matrix from cache
      
      inv <- solve(mat, ...)  # Step 4: Compute the inverse using `solve()`
      
      x$setInverse(inv)  # Step 5: Cache the computed inverse for future use
      
      inv  # Step 6: Return the inverse
}
