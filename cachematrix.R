## Function inorder to Cache the inverse of a matrix
## A special function 'MakeCacheMatrix' is defined

makeCacheMatrix <- function(x = numeric())
{
  ## Initially as nothing is cached, it is set it to NULL
  ## It holds the cached the value       
        
  cache <- NULL
        
  ## A function 'setMatrix' is defined to set the value of the Matrix

  setMatrix <- function(newValue)
  {
   ## '<<-' operator is being used to assign the value to 'x'
    
   x <<- newValue
   
   ## cache is set to NULL as a newvalue of the matrix is set
    
   cache <<- NULL
  }

   ## A function 'getMatrix' is defined to get the value of the Matrix

   getMatrix <- function() 
  {
   x
  }
  
  ## A function 'cacheInverse' is defined inorder the cache the value
    
  cacheInverse <- function(solve)
  {
   cache <<- solve
  }

  ## A function 'getInverse'is defined to get the cached value
  
  getInverse <- function()
  {
   cache
  }
        
  ## Return a list in which each named element is a fuction defined earlier

   list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## The function 'cacheSolve' calculates the inverse of a matrix created by makeCacheMatrix
cacheSolve <- function(y, ...)
{
 ## Getting the cached value

 inverse <- y$getInverse()

 ## If a cached value exists it will be returned
        
 if(!is.null(inverse))
   {
    message("Get the cached data")
    return(inverse)
   }
  ## If cached value doesn't exist then caclulate the inverse and store it in cache
     
   data <- y$getMatrix()
    
   inverse <- solve(data)
   
   y$cacheInverse(inverse)
        
   # Return the inverse matrix

        inverse
}