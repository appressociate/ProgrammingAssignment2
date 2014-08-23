## Matrix inversion is a costly computation especially for large matrices.
## makeCacheMatrix and cacheSolve are functions that can be used to compute 
## the inverse of a matrix and have the results cached. They are created to 
## avoid computing the inverse again and again if the results were already 
## computed and cached.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialising inverse to NULL
    inverse <- NULL
    
    ## set function assigns an input matrix y to variable x in another
    ## environment (cache) and assigns NULL to variable inverse in 
    ## another environment (cache)
    set <- function(y) {
    x <<- y
    inverse <<- NULL
    }

    ## get function returns the input matrix
    ## or the cached matrix if set before
    get <- function() x
    
    ## setInverse function assigns an input inverse matrix inv
    ## to variable inverse in another environment (cache)
    setinverse <- function(inv) inverse <<- inv

    ## getInverse function simply returns the input inverse matrix
    ## or the cached matrix if setinverse before
    getinverse <- function() inverse

    ## returns a list for each of the above functions -
    ## set, get, setinverse, getinverse
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function solve for the inverse 
## of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been solved. 
## If so, it gets the solution from the cache and skips the computation. 
## Otherwise, it solves for the inverse and sets the solution in the cache via 
## the setinverse function.
cacheSolve <- function(x, ...) {
  
  ## get the cached inverse matrix and assign it to variable inverse 
  inverse <- x$getinverse()
  
  ## if inverse is not null, it means two things:
  ## 1) the matrix has not changed by the set function in makeCacheMatrix
  ## 2) the inverse has been solved and cached
  ## in that case, simply return the cached solution
  if(!is.null(inverse)) {
    
    message("getting cached solution")
    return(inverse)
    
  }
  
  ## if inverse is null, we would need to solve for the inverse
  ## first, get the matrix to be inverted and assign it to variable data
  data <- x$get()
  
  ## next, cache the matrix to be inverted so that
  ## if it is changed, the solution will be re-computed
  x$set(data)
  
  ## now, solve for the inverse of the matrix
  inverse <- solve(data, ...)
  
  ## next, cache the solution so that if the matrix has not changed
  ## and that the solution has been computed before,
  ## the cached solution can be returned
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
