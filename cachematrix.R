## Filename: casheMatrix.R

## Calculates the inverse of a matrix which can be intensive. Once the inverse
## has been computed, we avoid repeating the calculation by storing the result
## in a special matrix object that stores both the original matrix and its
## inverse. 

## The makeCacheMatrix function below constructs a generalized matrix object
## that stores both itself and its inverse.  

makeCacheMatrix <- function(x = matrix()) {

## The determinant of the matrix x must be unequal to zero, otherwise, the
## inverse does not exist.
        
        inverseMatrix = NULL
        
        ## The following internal function can be accessed by the user  
        ## to create a generalized matrix object that the cacheSolve() function  
        ## can access to store and retrieve the matrix inverse once it has 
        ## been calculated.
        
        setMatrix = function(y) {
                
                # Assign values to the generalized matrix object
                # outside of this local environment 
                x <<- y
                inverseMatrix = NULL                
                }
        
        ## The generalized matrix object has four special 
        ## functions that the cacheSolve() function can access:
        
        ## 1. setMatrix()  - save the input matrix x in memory
        ## 2. getMatrix()  - retrieve the matrix x from memory
        ## 3. setInverse() - save the inverse in memory
        ## 4. getInverse() - retrieve the inverse from memory if the 
        ##                 inverse has already been calculated.
        
        getMatrix  = function() {x}
        setInverse = function(inverse) {inverseMatrix <<- inverse} 
        getInverse = function() {inverseMatrix}
        
        list(setMatrix  = setMatrix,  getMatrix  = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}



## The following function computes the inverse of a matrix if it has not been
## already been calculated. If the inverse has already been calculated, the
## inverse matrix is retrieved from cache memory using the getInverse()
## function of the generalized matrix object created by the makeCacheMatrix
## function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                
        inverse <-  x$getInverse()
                
        # This statement retrieves the previously calculated inverse
        # if it has already been calculated.
        if (!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
                
        #If inverse is null, calculate the inverse
        message("Computing the inverse. The computing time depends on the
                 size of the matrix.")
        matrix.data = x$getMatrix()
        inv <- solve(matrix.data, ...)
                
        # save the inverse in the cache
        x$setInverse(inv)
         
        inv
}