## Code for the week #2 programming assignment from the John Hopkins Cousera R-Programming course 

## makeCacheMatrix -- a function to create a matrix that has persists it value 
##                    and the inverse value. 

## cacheSolve      -- a function to get the  inverse value for a matrix that
##                    was previously created using the makeCacheMatrix function


# The makeCacheMatrix function is used to generate a 'special' matrix 
# that contains properties that persist the matrix and it's inverse value. 
# These properties are assigned through a functions (similar to methods in c classes)
# that get or set the relevant values. 

makeCacheMatrix <- function(x = matrix()) {
    
    matrixInv <- NULL  # defaults the inverse to null
    
    #define an internal function that sets the matrix content       
    
    setMatrix <- function (y)
    {   
        # populate the matrix with the value supplied in y      
        x <<- y
        # set the initial value of the inverse matrix to NULL
        matrixInv <<- NULL
    }  
  
    # define an internal function that gets the matrix content       
    
    getMatrix <- function()
    {
        # return the matrix to the caller       
        x      
    }
    
    # define an internal function that sets the matrix inverse property        
    # this will be called from the cacheSolve function if the matrixInv
    # value is Null
    
    setSolve <- function(invValue) 
    {
      # set the persisted value to the value passed into the function
      matrixInv <<- invValue
    }
    
    
    getSolve <- function() 
    {
      # return the persisted value 
      matrixInv
    }
    
    # create a list to store the functions 
    list(setMatrix = setMatrix, getMatrix = getMatrix, setSolve = setSolve, getSolve = getSolve)
    
}


## cacheSolve takes an input that is a matrix previously created using
## the makeCacheMatrix function. 
## if the invers of the matrix has already been cached it will return it
## if the inverse of the matrix hasn't been previously calcualted 
## the cacheSolve function will also calculate the value and cache it

cacheSolve <- function(x = matrix(), ...) 
{
    ## Function to return a matrix that is the inverse of 'x'
    inv <- x$getSolve()
    if (!is.null(inv))
    {
        message("using cached value")
        return(inv)
    }
    # this section will only execute if the inverse value has not been
    # previously calculated and cached.
    
    data <- x$getMatrix()    # get the matrix 
    inv <- solve(data) # calculate the inverse - note this will error if the matrix is not a square
    x$setSolve(inv)    # persit the value so that it doesn't need to be calcualted again
    inv                # return the value 
}
