
## Put comments here that give an overall description of what your
## functions do

## The following two functions cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<- inverse
  getinverse<-function() m
  mpm<<-list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  mpm
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x = matrix(), ...) {
  m<-mpm$getinverse()
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
  DataMatrix<-mpm$get()
  m<-solve(DataMatrix, ...)
  mpm$setinverse(m)
  m
}
