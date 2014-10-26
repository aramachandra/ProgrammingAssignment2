## Caching the Inverse of a Matrix

## Create makeCacheMatrix to contain a list to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix<-NULL
  set<-function(y){
    x<<-y
    inverse_matrix<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) inverse_matrix<<- solve
  getmatrix<-function() inverse_matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## The below function returns the inverse martix.
## First it checks if inverse matrix is available in cache.
## If not, it calcultes inverse of the matrix.

cacheSolve <- function(x=matrix(), ...) {
  inverse_matrix<-x$getmatrix()
  if(!is.null(inverse_matrix)){
    message("getting cached data")
    return(inverse_matrix)
  }
  matrix<-x$get()
  inverse_matrix<-solve(matrix, ...)
  x$setmatrix(inverse_matrix)
  inverse_matrix
}


## TESTING THE ABOVE FUNCTIONS
## a <- matrix(1:4, nrow = 2, ncol = 2)
## m = makeCacheMatrix(a)
## m$get()
## Output:
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## There is no cache when we run for the first time.
## cacheSolve(m)
## Output:
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## When we run the second time, we get the inverse matrix from the cache
## cacheSolve(m)
## Output:
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
