## Set of 2 functions to inverse matrix

## Create Cache Matrix
makeCacheMatrix <- function(x = matrix()) {
        elc <- NULL
        
        set <- function(y)
        {
                x <<- y
                elc <<- NULL
        }
        get <- function() x
        
        setreverse<- function(reverse) elc <<-reverse
        getreverse <- function() elc
        list(set = set, get = get,setreverse = setreverse,getreverse = getreverse)      
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        elc <- x$getreverse()
        
        if (!is.null(elc)) 
        {
                message("getting cached reververse matrix")
                return(elc)
        } else {
                elc <- solve(x$get())
                x$setreverse(elc)
        return(elc)
        }
}
