## Calculating the inverse of a matrix can be a computation intensive operation. 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}




##################
## 
## To eliminate the requirement
## of performming the computation repeatedly, a special function
## is included below that defines a "matrix" object that stores the
## inverse matrix in cache  memory. 


## This function creates a "matrix" object that can also store its 
## own inverse in cache memory. This function is very similar to one
## posted by Guangming Lang

makeCacheMatrix <- function(x = matrix()) {
        ##y <- x^{-1}
        
        ## The determinant of x must be not be equal to zero.
        ## Otherwise, the inverse does not exist.
        
        ## This is a constructor function of a generalized matrix
        ## object that returns a list of functions that enable the 
        ## the cacheSolve() function to save and retrieve the inverse of
        ## the matrix.
        
        ## The generalized matrix object has the following special 
        ## functions: 
        ##    1. setMatrix (save) the input matrix x
        ##    2. getMatrix (retrieve) the matrix from memory
        ##    3. setInverse (save) the inverse
        ##    4. getInverse (retrieve) the inverse from memory
        
        inverseMatrix = NULL
        setMatrix = function(y) {
                # Assign values to an object in an outside environment 
                x <<- y
                ##inv <<- NULL
        }
        getMatrix = function() {x}
        setInverse = function(inverse) {inverseMatrix <<- inverse} 
        getInverse = function() {inverseMatrix}
        list(setMatrix=setMatrix,     getMatrix=getMatrix,
             setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of a matrix if it not alrready
## calculated and included within the generalized matrix object above.
## This function retrieves the inverse matrix from cache memory using the 
## getInverse function of the generalized matrix object produc ed by the 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) { 
        ## Return inverse of matrix that was input to makeCacheMatrix()
        ## x is the output of makeCacheMatrix()
        
        inverse <-  x$getInverse()
        print("Here")
        
        # This statement retrieves the previously calculated inverse
        # if it has already been calculated.
        if (is.null(inverse)){
                #calculate the inverse
                message("Computing the inverse. The computing time depends on the
                         size of the matrix.")
                matrix.data = x$getMatrix()
                inv <<- solve(matrix.data, ...)
                
                # save the inverse in the cache
                x$setInverse(inv)
        } 
        inv
}