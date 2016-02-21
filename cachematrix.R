	## makeCacheMatrx and CacheSolve functions are used to calculate and
	## cache the inverse of a matrix. Once the inverse is cached subsequent
	## calls on the same matrix will not result in calculation of the
	## inverse. Instead, the cached value is returned
	makeCacheMatrix <-function(matr = matrix()){
		rev <- NULL
		get <- function() matr
		set <- function(m){
		## override the values of matrix and its inverse to NULL
			rev <<- NULL
			matr <<- m
		}
		getrev <- function() rev
		setrev <- function(m) rev <<-m
		## creating the list to subset the functions
		list(set=set,get=get,getrev=getrev,setrev=setrev)
	}
	## function to calculate inverse of a matrix stored in x
	## if the inverse is already available it retrieved from
	## the cache
	cacheSolve <- function(x,...){
		rev <- x$getrev()
		## if the inverse is available retrieve the cached value
		if(!is.null(rev)){
			message("getting cached data...")
			return(rev)
		}
		##calculate the inverse and return it
		matr <- x$get()
		rev <- solve(matr)
		x$setrev(rev)
		rev
	}
