## Put comments here that give an overall description of what your
## functions do
## the functions store an inverted matrix in a cached object and retrieve the inverted matrix,
## if it exists otherwise calculates the inverted matrix
## MakeCacheMatrix creates the cache object and its methods and 
## cacheSolve returns the inverted matrix and caches it if it does not exist
## 
## Write a short comment describing this function
## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
      x<<-y
      m<<-NULL
  }
  get<-function () x
  setsolve<-function (solve) m <<-solve
  getsolve<-function () m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Write a short comment describing this function
## gets the inverse of the object above from cache and if not available by calculation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  if(!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
