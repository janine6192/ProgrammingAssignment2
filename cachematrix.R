## Author: Janine Tillett
## ######################


## makeCacheMatrix: creates a matrix 'x' that is list that contains get and set functions....
makeCacheMatrix <- function(x = matrix()) {
 
  m <- NULL
  
  ## set: sets the value 'x' with value 'y' 
 set <-function(y){
   x <<- y
   m <<- NULL
 }
 
 ## get: get the value of 'x' matrix
 get <- function() x
 
 ## setinverse: set the value of the inverse
 setinverse <- function(solve) m <<- solve
 
 ## getinverse: get the value of the inverse
 getinverse <- function() m
 
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
 
}


##cacheSolve: Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    m <- x$getinverse()
    ## if mean is calculated; return the the inverse
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    
    ## calculate the inverse
    data <- x$get()
      m <- solve(data, ...)
      
      ## set the value of the inverse
      x$setinverse(m)
    
      ## return the inverse
      m
    
  }
