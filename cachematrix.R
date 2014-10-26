##Creates a matrix and a function which checks to see if there is a cached value for the solve function (inverse of the matrix), if not, solves for the inverse and stores the result. 

##Creates the matrix solved for in the cacheSolve function. Makes functions accessible in the global environment rather than just the makeCacheMatrix environment. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##Check to see if the value of m is null, if it's not, returns the cached data. 

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}