## makeCacheMatrix creates an object which contains a certain number of
## attributes that can be called later. The cacheSolve basically just
## outputs the inversed matrix by refering to the first function and reset
## the object value.



## This function creates an object by taking the argument matrix. Inside the function,
## it firstly creates an empty inversed matrix called inv. It has 3 other 
## functionns that can be called later from outside. "get" function get the 
## original matrix X. setInverse function change the stored value of inversed 
## matrix. getInverse function just get the stored value of inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function(){x}
    setInverse <- function(inversed){inv <<- inversed}
    getInverse <- function(){inv}
    list(get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function takes specific attributes from the mackCacheMatrix,
## and output the inversed matrix. It firstly get the stored value of matrix
## from the makeCacheMatrix function. Only if the value is NULL, it will continue
## the calculation. Otherwise it just output the cached data. if value is
## NULL, then it calls the get function from makeCacheMatrix, then use solve()
## to calculate. Then it calls the setInverse value from makeCacheMatrix to 
## change the stored value in the first function. So next time when it's called,
## the system will see inv not as NULL any more.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)    
    inv
}
