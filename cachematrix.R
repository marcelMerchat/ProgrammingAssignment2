## Calculating the inverse of a matrix can be computation intensive. Once 
## the inverse has been computed, we can eliminate repeating the calculation
## by storing the inverse in a special matrix object that stores both the 
## matrix and its inverse. 

## The makeCacheMatrix function below constructs a generalized matrix object
## that stores both itself and its inverse. the inverse from the
## generalized matrix object 

makeCacheMatrix <- function(x = matrix()) {

## The determinant of the matrix x must be not be equal to zero.
## Otherwise, the inverse does not exist.
        
        inverseMatrix = NULL
        
        ## The following function can be accessed by the user to create a
        ## generalized matrix object that the cacheSolve() function
        ## can access to store and retrieve the matrix inverse once it has 
        ## been calculated.
        
        setMatrix = function(y) {
                
                # Assign values to the generalized matrix object
                # outside this global environment 
                x <<- y
                inverseMatrix = NULL                
                }
        
        ## The generalized matrix object has three more special 
        ## functions that the cacheSolve() function can access:
        
        ## 1. setMatrix - save the input matrix x in memory
        ## 2. getMatrix - retrieve the matrix x from memory
        ## 3. setInverse - save the inverse in memory
        ## 4. getInverse - retrieve the inverse from memory if the 
        ##                 inverse has already been calculated.
        
        getMatrix  = function() {x}
        setInverse = function(inverse) {inverseMatrix <<- inverse} 
        getInverse = function() {inverseMatrix}
        
        list(setMatrix  = setMatrix,  getMatrix  = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}




## The following function below computes the inverse of a matrix if it has not
## already been calculated and stored within the generalized matrix object.
## If the inverse has already been calculated, this function retrieves the
## inverse matrix from cache memory using the getInverse function of the
## generalized matrix object created by the makeCacheMatrix function above.

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