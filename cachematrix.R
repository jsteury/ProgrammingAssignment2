## The purpose of these functions is to efficiently calculate the inverse of a square matrix. 
## The results are cached so that if the inverse has been previously calculated, 
## the function will go to the cached value instead of recalculating. 

## This function makeCacheMatrix returns a list of functions that save and retrieve a matrix to a cache and 
## save and retrieve the inverse of a matrix.

makeCachematrix <-function(x=matrix()) {
  inv <-NULL # initialize inverse to Null
  set <-function(y) {    #define set function
    x<<-y                 #set x to y in a new/specific environment
    inv <<- NULL
  }
  get <- function() x    #returns x as defined
  
  setsolve <- function(solve) inv<<-solve
  getsolve <- function() inv
  
  list(set=set,get=get,setsolve=setsolve, getsolve=getsolve)  #create the list of functions 
}


## This function cacheSolve retunrs a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)){
      message ("getting cached inverse")
      return(inv)
    }
    matr <- x$get()
    inv <- solve(matr)
    x$setsolve(inv)
  }
  
