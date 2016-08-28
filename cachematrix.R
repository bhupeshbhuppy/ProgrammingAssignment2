## The function has following properties
## return_type= list
## Example for use
## x<-makeCacheMatrix
## use the functions as x$set(y), x$get() etc.

makeCacheMatrix <- function(x = matrix()) {
  cache_inv<-NULL
  cache_mt<-NULL
  set<-function(arg_mt){
    cache_mt<<-arg_mt
    cache_inv<<-NULL
  }
  get<-function() {
    if(!is.null(cache_mt))
    return(cache_mt)
  }
    else{
      print("Matrix not set. Please set the matrix before use.")
    }
  setInverse<-function(inv){
    cache_inv<<- inv
  }
  getInverse<-function() cache_inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## Return a matrix that is the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  data_mt<-x$get()
  inv<-solve(data_mt)
  x$setInverse(inv)
  return(inv)
       
}
