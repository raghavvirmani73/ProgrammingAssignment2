## These functions take a matrix and create a new "matrix object"
## that allows us to store (cache) its inverse. Then, the inverse
## is either returned directly from the cache or is calculated
## and stored. Comments are frequently introduced for the sake
## of readability.

## Creates the new "matrix object" that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #This function sets the value of the matrix
  #to a value specified by the user. The inverse
  #is also reset.
  setmatrix <- function(y = matrix()){
    x <<- y
    inv <<- NULL
  }
  
  #Function that returns the matrix itself.
  getmatrix <- function(){
    x
  }
  
  #Sets the inverse of the matrix to a value
  #specified by the user.
  setinverse <- function(inverse){
    inv <<- inverse
  }
  
  #Function that retrieves the inverse of 
  #the matrix.
  getinverse <- function(){
    inv
  }
  
  #Returning our new "matrix object"
  list(set = setmatrix, get = getmatrix,
       setinv = setinverse, getinv = getinverse)
  
  
}


#This function returns the inverse of the matrix
#if it already exists. If not, it is calculated and 
#stored.
cacheSolve <- function(x, ...) {
  
  #Getting the inverse and checking if it has been
  #calculated beforehand.
  inv <- x$getinv()
  if(!is.null(inv)){
    print("Returning cached data.")
    return(inv)
  }
  
  #Calculating the inverse and storing it.
  matrixval <- x$get()
  inv <- solve(matrixval)
  x$setinv(inv)
  inv
  
}
