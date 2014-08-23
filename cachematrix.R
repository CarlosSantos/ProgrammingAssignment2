#################################################################################
## Coursera, Data Science Specialization by JHU. 
## Module 2 "R Programing" - Programming Assginment 2
## Author:         Carlos Santos
## Description:    This set of functions addresses the Programing assignment 2 objective:
##                 >> create the code to store in cached memeory the inverse of a given matrix for fast retrieval <<
##                 It contains 2 functions:
##                      makeCacheMatrix()       ## Creates an special R object which can store it´s inverse matrix value
##                      cacheSolve()            ## Uses the R object created through makeCacheMatrix() to obtain the inverse matrix 
##                                                 by calcularting it's value or by obtaining if from cached memory

## Function:    makeCacheMatrix()
## Description: This function creates an R object which can store the inverse of a given Matrix (Through Memory caching)
##              and provides a fast data recovery value in case the matrix has not changed
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverseMatrix) i <<- inverseMatrix
        
        getInverse <- function() i
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function:    cacheSolve()
## Description: Resolve the Inverse of a Marix for a Matrix created through: makeCacheMatrix() function
##              It contains the logic for calculating the Inverse of a Matrix in case this has not been 
##              Calculated yet or else obtaing it from the cache
cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data)
        
        x$setInverse(i)
        
        ## Return a matrix that is the inverse of 'x'
        i
}
