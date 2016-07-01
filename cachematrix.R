## Cache the inverse of a matrix:
## Below are the two function that are used to create
## a list with the matrix and its inversion matrix stored inside 

## First function create an object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<- function(y){
                x<<- y
                s<<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s<<- solve
        getsolve <- function() s
        list(set=set,get=get,setsolve=setsolve, getsolve=getsolve)
}


## 2nd function compute the inverse of the matrix above. 
#  if the inverse is't NULL, then retrieve the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)){
                message("getting the cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
