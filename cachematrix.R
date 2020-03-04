## Functions that can create special matrix that can cache its inverse and computes inverse of the special matrix



## Function for creating a special matrix object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(matrix){
                m <<- matrix
                a <<- NULL
        }
        get <- function(){
                m
        }
        setInverse<-function(inverse){
                a <<- inverse
        }
        getInverse <- function(){
                a
        }
        list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)
        
}


##Computing Inverse of the special matrix returned by above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if (!is.null(m)){
                return(m)
        }
        
        data <- x$get()
        m <- solve(data) %*% data
        
        x$setInverse(m)
        
        m
}
