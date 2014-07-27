## These functions compute and cache the inverse of a matrix.
## If the inverse has already been calculated and the matrix has not changed, 
## then the result is retrieved from the cache.
## 

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  im<-NULL ## cache for the inverse of 'x'
  ## definition of manipulation function
  set <- function(y) {
    x <<- y
    im <<-NULL
  }
  get <- function() x
  setinv <- function(invm) im <<-invm
  getinv <- function() im
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'    
  im <- x$getinv()      
  if(!is.null(im)) {    ## is cached
    message("getting cached data") 
    return(im) ## return cached
  }
  ## not cached, computes the inverse of 'x'
  data <- x$get()
  im <- solve(data) 
  x$setinv(im) ## save to cache
  im     ## return the inverse of 'x'       
}

