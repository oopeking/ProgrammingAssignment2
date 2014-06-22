## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL
  getx<-function() x
  getxinv<-function() xinv
  setx<-function(m=matrix()){
    x<<-m
    xinv<<-NULL
  }
  setxinv<-function(minv=matrix()){
    xinv<<-minv
  }
  list(getx=getx,getxinv=getxinv,setx=setx,setxinv=setxinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  xinv<-x$getxinv()
  if(!is.null(xinv))
  {
    message("Get the inverse matrix from cache")
    xinv
  }
  else
  {
    m<-x$getx()
    if(is.null(m))
    {
      message("The original matrix is null, please check the value")
    }
    else
    {      
      #Hereby, we only deal with the invertible matrix
      sm<-solve(m)
      x$setxinv(sm)
      sm
    }
  }
}
