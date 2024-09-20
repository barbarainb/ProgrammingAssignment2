

## programming assignment 2 - Bárbara Bastos

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

inverse_matrix <- NULL #having an empty vector to store the inverted matrix

 set <- function(y)
 {x <<- y                  #assigning x to a new variable y
 inverse_matrix <<- NULL } #having an empty vector reset cashed inverse
 
  get <- function()
    {return(x)}                #returning the matrix x

    setinverse <- function()
    {inverse_matrix <<- solve(x)}

      getinverse <- function()
      {return(inverse_matrix)}
      
      # Return a list of functions to access the matrix and its inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##load prevous function

makeCacheMatrix


cacheSolve <- function(x) {
  # Check if the inverse is already cached or not:
  mat <- x$getinverse()
  if (!is.null(mat)) {
    message("getting cached data")
    return(mat)  # Return the cached inverse
  }
  
  # If not cached, compute the inverse
  data <- x$get()  # Get the original matrix
  mat <- solve(data)  # Compute the inverse
  x$setinverse(mat)  # Cache the computed inverse
  return(mat)  # Return the computed inverse
}
