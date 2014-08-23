## makeCacheMatrix and cacheSolve work together to cache an inverted matrix for
## repeated use. This approach will cause your code to execute quicker as it
## keeps the system from reprocessing the solve command repeatedly.

## makeCacheMatrix
## x: any invertible square matrix
## return: cacheable matrix with corresponding set, get, setSolve, and getSolve extensions
## use: 
##    mcm <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y){
          x<<-y
          s<<-NULL
      }
      get <- function() x
      
      setSolve <- function(solve) s <<- solve 
      
      getSolve <- function() s
      list(set=set, get=get,setSolve = setSolve,getSolve=getSolve)
}


## CacheSolve
## x: makeCacheMatrix
## return: inverted matrix of x as computed or as previously stored
## use:
##    mcm <- makeCacheMatrix(matrix(1:4,2,2))
##    cacheSolve(mcm)

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getSolve()
        if(!is.null(s)){
              message("getting cached data")
              return(s)
        }
        
        data<- x$get()
        s<- solve(data)
        x$setSolve(s)
        s
  
}
