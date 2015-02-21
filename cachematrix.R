## The purpose of these functions is to efficiently calculate the inverse of a 
## square matrix. The results are cached the first time a matrix is inversed so 
## that if the inverse has been previously calculated, the function will go to
## the cached value instead of recalculating. 

## This function makeCacheMatrix returns a list of functions that both saves and
## retrieve a matrix to/from the cache, and saves & retrieves the matrix inverse 
## to/from the cache.

makeCachematrix <-function(x=matrix()) {
    inv <-NULL             # initialize inverse to Null
    set <-function(y) {    #define set function
    x<<-y                  #set x to y in a new/specific environment
    inv <<- NULL
  }
    get <- function() x    #returns x as defined
  
    setsolve <- function(solve) inv<<-solve
    getsolve <- function() inv

  ##create the list of functions 
    list(set=set,get=get,setsolve=setsolve, getsolve=getsolve)  
}


## This function cacheSolve retunrs a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)){  #if the inverse has been cached, then retrieve it
      message ("getting cached inverse")
      return(inv)
    }
## otherwise, get the matrix data and calculate the inverse
    matr <- x$get()  
    inv <- solve(matr)
    x$setsolve(inv)
  }
  
