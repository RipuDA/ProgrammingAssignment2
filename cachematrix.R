## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object and retruns a list of fucntions
## for setting the value of matrix, getting the value of matrix
## setting the value of inverse of matrix
## getting the value of inverse of matrix

## Example: How to use the function
## Create a matrix x
## x<-matrix(c(7,0,-3,2,3,4,1,-1,-2),nrow=3,ncol=3)
## Call the function as follows
## y<-makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
  
  ## Creating a local variable m and initializing it to null
  m <- NULL
  
  ## This function sets the value of matrix
  setMatrix <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  ## This fucntion gets the value of matrix
  getMatrix <- function() x
  
  ## This function sets the inverse of a matrix
  setMatInverse <- function(matInverse) m <<- matInverse
  
  ## This function gets the inverse of a matrix
  getMatInverse <- function() m

  ## returns list of function
  list(setMatrix=setMatrix, getMatrix=getMatrix, setMatInverse=setMatInverse,
       getMatInverse=getMatInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

## Example: How to use the function
## Create a matrix x
## x<-matrix(c(7,0,-3,2,3,4,1,-1,-2),nrow=3,ncol=3)
## Call the function as follows
## y<-makeCacheMatrix(x)
## Call the function as cacheSolve(y)

cacheSolve <- function(x, ...) {
  
        ## Check whether Inverse is present in cache or not
          m <- x$getMatInverse()
          if(!is.null(m))
          {
            message("Getting the inverse of matrix from cache")
            return(m)
          }
        ## If not present in cache, then matrix will be retrieved first
        dataMatrix <- x$getMatrix()
        
        ## Check if the matrix has inverse or not
        if(class(try(solve(dataMatrix),silent=T))=="matrix")
        {
          m <- solve(dataMatrix)
          x$setMatInverse(m)
          ## Return a matrix that is the inverse of 'x'
          m
        }
        else
        {
          message("The matrix supplied doesnot have inverse")
        }
        
        
}
