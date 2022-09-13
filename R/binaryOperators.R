
#' Binary Operators with Implicit Expansion
#' 
#' @description
#' Modified versions of binary operators that support implicit expansion.
#' Arguments are passed to [`mmapply()`] with the corresponding element-wise binary operator.
#' For instance, `x %m+% y` is equivalent to `mmapply('+', x, y)`.
#' 
#' @param x,y Arrays or objects that can be coerced to arrays using [`as.array()`].
#' 
#' @return The result of `mmapply('XXX', x, y)`,
#' with `XXX` replaced by the corresponding element-wise binary operator.
#' 
#' @examples 
#' x <- c(1,2,3)
#' y <- t(c(4,5))
#' x %m+% y
#' 
#' m <- matrix(3*(1:12)^2, 3, 4)
#' cm <- t(colMeans(m))
#' m %m-% cm
#' 
#' @seealso 
#' [`mmapply`]
#' 
#' @rdname BinaryOperators
#' @aliases BinaryOperators
#' @export
`%m+%` <- function(x, y){
    mmapply(`+`, x, y)
}

#' @rdname BinaryOperators
#' @export
`%m-%` <- function(x, y){
    mmapply(`-`, x, y)
}

#' @rdname BinaryOperators
#' @export
`%m*%` <- function(x, y){
    mmapply(`*`, x, y)
}

#' @rdname BinaryOperators
#' @export
`%m/%` <- function(x, y){
    mmapply(`/`, x, y)
}