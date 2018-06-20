## These functions cache the inverse of a matrix, assuming that the matrix provided is always 
## invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   
  m <- NULL #Initializacion 
  set <- function(y) {
    x <<- y #Assign the imput argument to x in the parent environment.
    m <<- NULL #Assign NULL to m.inverse in parent envorinment.
  }
  get <- function() x #Define the getter matrix x
  setinverse <- function(solve) m <<- solve #define the setter for the inverse.
  getinverse <- function() m #Define the getter for the inverse.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #The list return the last functions to the parent environment.
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #Attempts to retrieve the inverse from the object passes in as the argument.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } #Check if the result isn't NULL, if that is correct, I get a valid, CacheSolve (inverse)
  data <- x$get()
  m <- solve(data, ...) #Execute solve (inverse) function
  x$setinverse(m)
  m #Return the inverse matrix
  }
