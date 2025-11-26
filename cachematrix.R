## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a storage for the inversed matrix and include methods
## to get the original matrix, set the original matrix, get the inversed matrix,
## set the inversed matrix
## - set(): update the matrix
## - get(): retrieve the matrix
## - setinverse(): store the computed inverse
## - getinverse(): retrieve the cached inverse
makeCacheMatrix<-function (x=matrix()){
	inverseMatrix <-NULL
	
	set <- function(y){
		x <<-y
		inverseMatrix <<-NULL	
}

	get <- function(){
		x
}

	setinverse <- function(inversedMatrix){
		inverseMatrix <<- inversedMatrix
}
	getinverse <- function(){
		inverseMatrix
}
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
## This function check to see if the input matrix's inversion is computed
## If the inversion is stored in makeCacheMatrix then return it, if it is not
## compute via solve(), stores it and returns it
cacheSolve <- function(x, ...){

	cachedInverse <- x$getinverse()

	if(!is.null(cachedInverse)){
		message("matrix inversion is computed")
		return(cachedInverse)
}

	tempData <- x$get()

	computedInverse <- solve(tempData, ...)

	x$setinverse(computedInverse)

	computedInverse
}
