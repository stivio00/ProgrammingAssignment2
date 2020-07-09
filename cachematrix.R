## Assg 2 by Stephen Krol
## Put comments here that give an overall description of what your
## functions do

## This function cache the matrix inverse in m hided variable
## m is only available troug get function
## and redirects mean to solve
makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

##
## This fucntion will retrieve the m value(or atrribute) from the list
cacheSolve <- function(x, ...) {
  m <- x$getsolve() #solved value
  
  if(!is.null(m)) {
    message("Resolviendo matriz inversa (English: Doing inverse)")
    return(m)
  }
  
  m <- solve(x$get() , ...)  #getter (lexical scoping the m value)
  x$setsolve(m)              # Chaching the  value
  m
}

## Will run a test with a matrix nXn
test <- function(n=4){
  x = matrix(sample(1:50, n*n), n, n)
  
  matrix = makeCacheMatrix(x)
  
  cacheSolve(matrix)
}