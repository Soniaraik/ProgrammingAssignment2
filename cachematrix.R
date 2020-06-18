 makeCacheMatrix <- function(x = matrix()) {
    z<- NULL                            
    set <- function(y) {                    
      x <<- y                          
      z<<- NULL                      
    }
    get <- function() x                    
   
    setinverse <- function(inverse) z<<- inverse  
    getinverse <- function() z        
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  }
  cacheSolve <- function(x, ...) {
    z<- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    z<- solve(data, ...)
    x$setinverse(inv)
    inv
  }
