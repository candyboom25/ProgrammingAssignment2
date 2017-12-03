## Two functions that are used to create a special object that cache the inverse of a Matrix

## Function creates a special Matrix object tht can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<- function(y){
                x<<- y
                m <<- NULL
        }
        get<- function()x
        setinverse<- function(inverse) m <<- inverse
        getinverse<- function() m
        list(set = set, get= get,
             setinverse = setinverse,
             getinverse=getinverse)
        
}

## Function computes the inverse of the special Matrix returned by the function above. If inverse has been calculated, the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<- x$getinverse()
        if(!is.null(m)){
            message("getting cached date")
            return (m)
        }
        data<- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
}

