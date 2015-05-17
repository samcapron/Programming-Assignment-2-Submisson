#create function called makeCacheMatrix. This function returns a list of functions -
#setMatrix, getMatrix, setInverse, getInverse. In list will be the stored
#cache inverse. If it is Null then the cacheSolve function will recalculate it. 

makeCacheMatrix <- function(x = numeric()) {
  
    # empties cache
    cache <- NULL
    
    #elements of matrix
    
    # setMatrix - set value of matrix
    setMatrix <- function(newValue) {
      
        x <<- newValue
        # since the matrix is assigned a new value, null the cache
        cache <<- NULL
        
    }
    
    # getMatrix - returns the value of the matrix x
    getMatrix <- function() x
      
    # set the cached value (inverse of the matrix x)
    setInverse <- function(solve) cache <<- solve
   
    #get the cached value (inverse of the matrix x)
    getInverse <- function() cache
  
    # return a list of fuctions of inputted matrix x
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Create a fucntion called caheSolve. The following function calculates
#the inverse of the matrix created with makeCacheMatrix. If the cache is null 
#then it will recalculate it. 

cacheSolve <- function(m, ...) {
  
  # if a cached value is not null then return it
    if(!is.null(m$getInverse())) {
      
        message("getting cached data")
        
        return(m$getInverse())}
    
    #if it is NULL then reclaculate inverse with Solve function
    else {
      
        # otherwise get the matrix, calculate the inverse and store it in
        # the cache
        
        #call matrix - first element of list
        matrix <- m$getMatrix()
        
        #solve function - gives inverse
        inverse <- solve(matrix)
        
        #fill the cache with calculated inverse for matrix
        m$setInverse(inverse)
        
        # display the new inverse
        inverse
        
    }
}