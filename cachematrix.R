###############################################################################
## Coursera R programming course - RPeng
## created December 21 2014
#### Goal:
## A pair of function that will cache  the inverse of a matrix rather 
## than copute it repeatedly.
## 

## This function takes a matrix "x" as an input,
## the output is a an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## input is an invertible matrix
## set inv to NULL, re-sets to NULL very time 'makeCacheMatrix' is called
inv <- NULL   
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() {x}   ## this function returns the value of the origianl matix, 'x"
## this function is called by 'cacheSolve' the first time cacheSolve is accessed
## it will store the value of 'inv' using superassigment (ie '<<-')
set.inv <- function(solve) {inv <<- solve} 
## this will return the cached value to 'cacheSolve' on subsequent times
##  cacheSolve is accessed
get.inv <- function() {inv}
## this list is re-created each time makeCacheMatrix is called
##  
list(
  set = set, get = get,
  set.inv = set.inv,
  get.inv = get.inv)
}


## This function is the second of the pair.  As an input is uses the output
## of the makeCacheMatrix function.  The outut of cacheSolve is the inverse
## of the original matrix taht was input to the makeCacheMatrix function.
## if cacheSolve is returning a cached value of the inverse, it will also
## output a message 'getting cached data'

cacheSolve <- function(x, ...) { ## input is the output of 'makeCacheMaktrix,
## **NOT** the same input as used for makeCacheMatrix
  inv <- x$get.inv()  ## pulls the value of 'get.inv' from the output of
                      ## of makeCacheMatrix
  if(!is.null(inv)) { ## if the get.inv list memeber is *NOT* NULL, then
                      ## return the already cached value of 'inv" 
    message("getting cached data")
    return(inv)
    }
  ## if the value of get.inv is NULL, ie we havent accessed makeCacheMatrix
  ## before, then create a matrix using the original input to makeCacheMatrix
  ## name this 'matrix1'
  matrix1 <- x$get()
  ## solve fo r the inverse of matrix1 using the 'solve' function
  inv <- solve(matrix1, ...)
  ## change the value of inv in makeCacheMatrix from NULL to the newly
  ## calcualted inv
  x$set.inv(inv)
  ## return the inverse of the matrix initialy input to makeCacheMatrix
  inv
}
