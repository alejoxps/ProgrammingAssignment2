##The functions below let you to create a special matrix that can be use to keep cached
##the value of its inverse, calculate it and retrieve it when that will be necessary


##this function  creates a special "Matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function()x
    setinverse<-function(inverse)inv<<-inverse
    getinverse<-function()inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##this function allows you to get the inverse of a special matrix x,  it first checks 
##to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inverse <- x$getinverse()#Getting the inverse of x
      if(!is.null(inverse)) {##If the inverse exists
        message("getting cached data")
      return(inverse)##Return the saved inverse matrix
      }
    data <- x$get() ##Getting the matrix
    inverse <- solve(data)##Calculating the inverse of x
    x$setinverse(inverse)##Setting the inverse
    inverse #Return the inverse of X
}
##Note: It is not necessary to check if the matrix has changed, because if the set function
##is used to set a new value of the matrix, then the inverse of x will be set to NULL.