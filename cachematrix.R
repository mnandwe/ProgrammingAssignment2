## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes 1 argument, a matrix, and returns a list
## with the functions to get and set the matrix and inverse.


makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        
        setinverse <- function(inverse) i<<-inverse
        getinverse <- function() i
        
        list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
}


## cacheSolve returns the inverse of a matrix set in makeCacheMatrix
## it checks if the list created in makeCacheMatrix has an inverse matrix, i. 
## If there is no inverse, then is computes and returns the inverse

cacheSolve <- function(x,?solve ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setinverse(i)
        i
}
