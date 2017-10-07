## This file is written in partial fullfillment of the course R programming in Coursera
## by GitHub user: revathyv95

## the function makeCacheMatrix gets a matrix as an input, sets the value and get the value of the matrix.
## it sets and gets the inverse matrix. the matrix object can cache its own object.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set<-function(y){
                x<<-y 
                inv<<-NULL
                }
        get<-function()x
        setinverse<-function(inverse)inv<<-inverse
        getinverse<-function()inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)       
                
}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
-# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
-# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
-# and set the invertible  matrix by using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
        
}
