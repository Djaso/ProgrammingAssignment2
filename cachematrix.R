## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special matrix where x is an matrix 
# We get the value and set them by making use of getters and setters
# that are define within the function.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # getters and setters
  
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse 
  get_inverse <- function() m
  
  
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  

}


## Write a short comment describing this function

# This function computes the inverse of a special matrix
# returned by the function define above. Where if the 
# inverse was already calculated then the cachesolve get the 
# inverse from th cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$get_inverse()
  
  if(!is.null(m)){
    message("getting cached data.")
    return(m)
    
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$set_Inverse(m)
  m
}


