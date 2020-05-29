## This couple of function verifie if the inverse of a matrix was previous calculated and its save in the cache.



makeCacheMatrix <- function(x = matrix()) { ## This function generate a list of functions that help to save a inverse of a matrix in the cache
    m <- NULL ##generate a NULL object
    set <- function(y) { ## Function that save two objects in a different environment of the function
        x <<- y ## save the object "y" in a new object call "x" in a different enviroment
        m <<- NULL ## save the NULL object m in a different enviroment
    }
    get <- function() x ##function that get the value of x
    setinverse <- function(inverse) m <<- inverse ## function that save the inverse in a different environment
    getinverse <- function(m) solve(m) #generate the inverse of a matrix
    list(set = set, get = get,   ##create a listr of the functions that could be used in the other function
         setinverse = setinverse,
         getinverse = getinverse)
    
}



cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    m <- x$getijnverse() ##save theresult of the getinverse function of the list in the object m
    if(!is.null(m)) { ##loop that verifie if m is a NULL object, in case that isn´t NULL, make the actions inside.
        message("getting cached data") ##put the message in the screen
        return(m) ##return the value of m that is in the cache
    }
    data <- x$get() ##save the result of the get function in the object data
    m.calculated <- solve(data, ...) ##calculated the inverse of the matrix
    x$setinverse(m.calculated) ##developed the setinverse function that save the inverse in an object in a different environment
    m.calculated # return the object that is the inverse of the matrix
}
