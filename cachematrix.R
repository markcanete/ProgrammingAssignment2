# This will Creates a matrix that can cache it's inverse, if the inverse has already been
#calculated before, the Cached inverse is return
# 
#
# Returns:
# A matrix with functions to get/set value & get_matrix/set_matrix inverse

makeCacheMatrix <- function(x = matrix()) {

        inverse_math <- NULL
       
        ##function get_matrix and set_matrix for matrix 
       get_matrix <- function() x
          set_matrix <- function(y){
            x <<- y
          inverse_math <<- NULL
  ## function getinv for inverse of matrix                
  getinv <- function() inverse_math
  setinv <- function(inverse) inverse_math <<- inverse
  
  ## return list of function for matrix
    list(get_matrix=get_matrix, set_matrix = set_matrix, getinv=getinv,setinv=setinv)
}


## Computes the inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_math <- x$getinv()
  
  if(!is.null(inverse_math)){
    message("inverse is cached")
    return(inverse_math)
  }
  m <- x$get_matrix()
  inverse_math <- solve(m,...)
  
  x$setinv(inverse_math)
  
  return(inverse_math)
}     
        
}
