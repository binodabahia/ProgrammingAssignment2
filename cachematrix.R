#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              #Initializes the variable
  
  set <- function(y) {     #Set the matrix
    x <<- y                #Assigns y to x in another environment
    inv <<- NULL
  }
  get <- function() x      #Get the matrix
  setinverse <- function(my_inverse) inv <<- my_inverse  #set the inverse of the matrix in another environment (cache)
  getinverse <- function() inv                           #get the inverse of the matrix
  list(set = set, get = get,                             #return a list 
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
 
  inv <- x$getinverse()  #gets the inverse of the matrix if it has been calculated
  if(!is.null(inv)) {    #if the inverse has already been calculated returns the value cached
    message("getting cached data")
    return(inv)
    
  }
  data <- x$get()        #if the inverse hasn't been calculated , get the matrix  and...
  inv <- solve(data, ...)# calculates its inverse
  x$setinverse(inv)      # cache the inverse
  inv
}




