## Put comments here that give an overall description
# Cousera class R Programming
# Solution to Programming Assignment 2: Lexical Scoping
# write two R function is able to cache potentially time-consuming computations


# Create a special object that store a matrix and cache's its inverse
# where:
# mat is a matrix to be inverted
# inversemat is the inverse the "mat"
makeCacheMatrix <- function(mat = matrix()) {
    inversemat <- NULL
    
    set <- function( matrixnew ) {
        mat <<- matrixnew
        inversemat <<- NULL
    }
    get <- function() mat
    setinverse <- function( inversenew ) inversemat <<- inversenew
    getinverse <- function( ) inversemat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function( pmatrix, ... ) {
    
    matrix <- pmatrix$getinverse()
    
    if( !is.null( matrix ) ) {
        message("getting inverse of the Matrix")
        return( matrix )
    }
    
    data <- pmatrix$get()
    matrix <- solve(data)
    pmatrix$setinverse(matrix)
    matrix
}
