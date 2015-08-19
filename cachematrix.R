## This file contains two functions that will help in computing an inverse of a matrix and caching it for later use.
## Learn about the <<- assignment operator at the below website
## https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/

## makeCacheMatrix(matrix)
## Function used to create a cacheable matrix. 
## This has a getter/setter pair which is used for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	## inverse is the variable that will cache the inverse matrix
	## we initialize is to null
	
	inverse<-NULL

	## set(matrix)
	## setter function for the input matrix
	set<-function(input){
		x <<-input
		inverse <<- NULL
	}

	## get()
	## getter function for input matrix
	get <- function(){
		x
	}

	## setinverse(matrix)
	## sets the value of inverse of the input matrix
	## merely a setter function, does not do any calculation
	setinverse <- function(inversematrix){
		inverse <<-inversematrix
	}

	## getinverse()
	## getter function for the inverse of the input matrix
	getinverse<-function(){
		inverse
	}

	## return a list of getter and setter methods as a result of object creation
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve(matrix)
## Takes a matrix as an input and returns an inverse of it
## If an inverse of an input matrix exists in its cache, it returns the result, without doing an inverse
## If an inverse of an input matrix does not exist in its cache, it performs an inverse, caches and returns the result

cacheSolve <- function(x, ...) {

	## First retrieve the inverse from cache
	inversematrix <- x$getinverse()

	## If inverse exists in cache, it will not be null
	if(!is.null(inversematrix)){
		message("getting cached inverse matrix")
		return(inversematrix)		
	}

	## If cache is null, then calculate inverse
	data<-x$get()
	inversematrix <- solve(data, ...)
	
	## store result in cache, so next time result can be obtained from cache
	x$setinverse(inversematrix)
	inversematrix
}
