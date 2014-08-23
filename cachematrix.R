## The following functions calculate the inverse of a matrix and saves it
## to the cache, so the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

##This function creates a special "matrix" object that is really a list
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # "i" will store the cached inverse matrix
  i<-NULL
  
  # Setter for the matrix
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  # Getter for the matrix
  get<-function() x
  
  # Setter for the inverse
  setinverse<-function(solve) i<<- solve
  
  # Getter for the inverse
  getinverse<-function() i
  
  #returns the matrix containing all of the functions just defined
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
 
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), 
##then should retrieve the inverse from the cache

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i<-x$getinverse()
  
  # If the inverse is already calculated, return it
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  # If the inverse is not yet calculated, we calculate it
  data<-x$get()
  i<-solve(data, ...)
  
  # Cache the inverse
  x$setinverse(i)
  
  # Return it
  i
  
}
