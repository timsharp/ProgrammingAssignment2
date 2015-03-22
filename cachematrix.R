## Programming Assignment #2 Lexical Scoping
## FUNCTION: makeCacheMatrix
## DESCRIPTION:creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(v_matrix = matrix()) { 
  v_inverse <- NULL 
  set       <- function(x) { 
    v_matrix     <<- x    ;                 
    v_inverse    <<- NULL ; 
  } 
  
  get    <- function()    return(v_matrix)         ; 
  setinv <- function(inv) v_inverse <<- inv        ; 
  getinv <- function()    return(v_inverse)        ; 
  
  return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
} 

## Programming Assignment #2 Lexical Scoping
## FUNCTION: cacheSolve
## DESCRIPTION:This function computes the inverse of the special
##            "matrix" returned by makeCacheMatrix.
##             If the inverse has already been calculated 
##            (and the matrix has not changed), then the cachesolve
##             should retrieve the inverse from the cache.

cacheSolve <- function(v_matrix, ...) { 
  v_inverse <- v_matrix$getinv() 
  if(!is.null(v_inverse)) { 
    message("get the data in the cache")  ## Return a matrix that is the inverse of 'x'
    return(v_inverse) 
  } 
  data <- v_matrix$get() 
  v_inverse <- solve(data, ...) 
  v_matrix$setinv(v_inverse) 
  return(v_inverse) 
} 