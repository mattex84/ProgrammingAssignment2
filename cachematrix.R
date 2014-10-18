
## This function define a list of function that allows to store in cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) 
{
		m <- NULL
		
		## Define a function that store a matrix in the cache.
		set <- function(y)
		{
			x <<- y
			m <<- NULL		
		}
		
		## Define a function that return a matrix
		get <-  function() 
		{
			x
		}
		
		## Define a function that store a inverse of matrix in the cache.
		setInverse <- function(z)
		{
			m <<- z	
		}
		
		## Define a function that return the inverse of a matriche from the cache
		getInverse <-  function()
		{
			m
		}
		
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
		
}


## This function check if the inverse of a matrix is already cached: 
## if YES, the function only returned the stored matrix
## if NO the funcion put the inverse of the matrix in the cache

cacheSolve <- function(x, ...) 
{
        ## Try to obtain the Reverse matrix
        y <- x$getInverse
        
        ## If the reverse matrix is already cached it is simply returned
        if(!is.na(y))
        {
        	message("loading cached data")
        	return(y)
        }
        ## If the reverse in not cached it is chached and than returned.
        else
        {
        	park <- x$get()
        	z <- solve(park)
        	x$setInverse(z)
        	z
        }
        
}
