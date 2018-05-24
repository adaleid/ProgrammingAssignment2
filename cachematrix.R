## the main pupose is to cache the inverse of matrices that has been previoulsy calculated



## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function (y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inver <<- inverse
  
  getinverse <- function() inver
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}





## this function computes the inverse of matrices returned by makeCacheMatrix above. If the inverse has already been calculated, cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  
  if(!is.null(inver)) {
    
    message("getting cached data.")
    
    return(inver)
    
  }
  
  data <- x$get()
  
  inver <- solve(data)
  
  x$setinverse(inver)
  
  inver 
}
