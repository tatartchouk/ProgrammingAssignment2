## Creating an object wich can be used like a matrix
## the difference is that if the object has not changed since 
## previous request for the inverse, we use previously computed one 




## take a matrix, return object having set and get fonctions
## for matrix and for its inverse in the environement from wich it is called
makeCacheMatrix <- function(x = matrix()) {
    iM <- NULL
    set <- function(y){
        x <<- y
        iM <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) iM <<- inverse
    getInverse <- function() iM
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    iM <- x$getInverse()
    if(!is.null(iM)) message("getting cached data")
    else { ## if there is no inverse in the cashe we compute it
        data <- x$get()
        message("computing inverse matrix")
        iM <- solve(data)
        x$setInverse(iM)
    }
    return(iM)
}


## ho to use the code:
## produce some random square matrix for example
##  n<-5
## m5 <- replicate(n, rnorm(n)) 
## now making "cached matrix"from it 
## im5 <- cacheSolve(cm5)
## we can see the message:
## "computing inverse matrix"
## the inverse was not there and it had to compute, 
## executingthe same command once again: 
## im5 <- cacheSolve(cm5)
## computing inverse matrix
## we see that this time precomputed value is used
