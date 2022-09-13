
#' Row and Column Vectors
#' 
#' @description
#' These functions coerce an object to a 2-dimensional [`array`] in the shape of a 
#' row or column vector, i.e. an array with dimensions `c(1,d)` or `c(d,1)`
#' respectively, where `d=length(x)`.
#' 
#' @param x An object that can be passed to [`array()`].
#' @param USE.NAMES If `TRUE` and `x` has only one non-singular dimension, its names are preserved.
#' 
#' @return A row or column vector with the data from `x`.
#' 
#' @examples 
#' x <- c(a=1, b=2, c=3)
#' as.mColVector(x)
#' 
#' y <- matrix(1:12, 3, 4)
#' as.mRowVector(y)
#' 
#' @rdname RowAndColumnVectors
#' @aliases RowAndColumnVectors
#' @export
as.mRowVector <- function(x, USE.NAMES = TRUE){
    dNames <- NULL
    if(USE.NAMES){
        dNames <- getVectorNames(x, 2)
    }
    array(x, c(1, length(x)), dimnames = list(NULL, dNames))
}

#' @rdname RowAndColumnVectors
#' @export
as.mColVector <- function(x, USE.NAMES = TRUE){
    dNames <- NULL
    if(USE.NAMES){
        dNames <- getVectorNames(x, 1)
    }
    array(x, c(length(x), 1), dimnames = list(dNames, NULL))
}

getVectorNames <- function(x, preferredDim=NULL){
    if(is.vector(x)){
        return(names(x))
    }
    if(is.array(x)){
        d <- dim(x)
        len <- length(x)
        dNames <- dimnames(x)
        # Abort if array is not named, empty, or has multiple large dimensions:
        if(is.null(dNames) || len == 0 || sum(d>1) > 1){
            return(NULL)
        }
        # Check preferred dims first:
        dims <- seq_along(d)
        if(!is.null(preferredDim)){
            dims <- c(preferredDim, dims)
        }
        # Return names of first "large" dimension:
        for(i in dims){
            if(length(dNames[[i]]) == len){
                return(dNames[[i]])
            }
        }
        return(NULL)
    }
    return(NULL)
}