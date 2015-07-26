## TITLE: Programming Assignment 2: Lexical Scoping
## Autor: Juan Sebastian Salazar Aguirre

## The code is commented to understand better the way the 
## functions work

## the makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.  Remember that cache is a memory of computers
## that is characterized by the fast access it provides to processed
## data

## makeCacheMatrix takes a matrix as argument, if the object is not
## a matrix it coerces it to be one.

makeCacheMatrix <- function(matrix = matrix()) {
        
        ## inverse's initial value is NULL, because it has not been
        ## calculated
        
        inverse <- NULL
        
        ## Function to set the Matrix, it's an closure function
        
        setMatrix <- function(y) {
                
                matrix <<- y
                inverse <<- NULL
                
        }
        
        ## Function to get the matrix, takes the matrix argument.
        ## This is an closure function too
        
        getMatrix <- function() matrix
        
        ## Function to set the inverse matrix, it needs the inverse
        ## Matrix as an argument to work and it's a closure function
        
        setInverseMatrix <- function(inverseMatrix) inverse <<- inverseMatrix
        
        ## Function to get the inverse matrix, it takes the value
        ## of inverse which is NULL when there's no cache.  It's
        ## another closure function.
        
        getInverseMatrix <- function() inverse
        
        ## List to be returned, it has 4 elements
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
        
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## The special thing about this function is that if 
## the inverse has been calculated it retrieve the
## inverse from the cache memory, so there's no
## consuming computation required

## cacheSolveInverse function takes matrix as an argument

cacheSolveInverse <- function(matrix, ...) {
        
        ## Takes the inverse value if it has already been
        ## calculated, if it's values is not NULL
        
        inverse <- matrix$getInverseMatrix()
        
        if(!is.null(inverse)) {
                
                message("getting cached data")
                return(inverse)
                
        }
        
        ## If inverse value is NULL, this function takes the
        ## matrix as data using the getMatrix() closure
        ## function of the makeCacheMatrix() function.
        
        data <- matrix$getMatrix()
        
        ## Calculate the inverse using solve()
        
        inverse <- solve(data, ...)
        
        ## Set inverse matrix using setInverseMatrix closure
        ## method and returning inverse
        
        matrix$setInverseMatrix(inverse)
        inverse
        
}

## Uncomment to run

##matrix1 <- matrix(1:4, 2, 2)
##cacheMatrix <- makeCacheMatrix(matrix1)
##cacheMatrix$getMatrix()
##cacheSolveInverse(cacheMatrix)
