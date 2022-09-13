

#' Array Creation
#' 
#' Convenience functions that create an array full of identical entries.
#' 
#' @param ... Numbers or numeric vectors, passed to [`c()`] and then used as `dim`
#'  argument in a call to [`array()`].
#' 
#' @return The result of `array(XXX, c(...))`, where XXX is `0`, `1`, [`TRUE`], [`FALSE`], or [`list()`], respectively.
#' 
#' @examples 
#' mZeros(2, 3)
#' 
#' mOnes(c(1, 2, 3))
#' 
#' mTRUE(c(1, 3), 2)
#' 
#' mFALSE(5)
#' 
#' @rdname ArrayCreation
#' @aliases ArrayCreation
#' @export
mZeros <- function(...){
    array(0, c(...))
}

#' @rdname ArrayCreation
#' @export
mOnes <- function(...){
    array(1, c(...))
}

#' @rdname ArrayCreation
#' @export
mTRUE <- function(...){
    array(TRUE, c(...))
}

#' @rdname ArrayCreation
#' @export
mFALSE <- function(...){
    array(FALSE, c(...))
}

#' @rdname ArrayCreation
#' @export
mNULL <- function(...){
    array(list(), c(...))
}
