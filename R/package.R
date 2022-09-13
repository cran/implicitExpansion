
#' Implicit Expansion
#' 
#' This package implements a feature known as "Broadcasting" in Python 
#' (see e.g. [here](https://numpy.org/doc/stable/user/basics.broadcasting.html))
#' and as "Implicit Expansion" in Matlab
#' (see e.g. [here](https://ch.mathworks.com/help/matlab/matlab_prog/compatible-array-sizes-for-basic-operations.html)).
#' In operations involving multiple arguments of type [array]
#' (or [vector], [matrix], [list]) with mismatching dimensions, any argument is
#' repeated along its dimensions of size (exactly) 1, as often as necessary to
#' match the other argument(s).
#' 
#' Below are some examples that illustrate possible operations using implicit expansion.
#' For detailed explanations of the behavior, see the corresponding docs for
#' [Python](https://numpy.org/doc/stable/user/basics.broadcasting.html)
#' and
#' [Matlab](https://ch.mathworks.com/help/matlab/matlab_prog/compatible-array-sizes-for-basic-operations.html).
#' 
#' Dimensions of size 0 are ok, as long as all other arrays are also 
#' of size 0 or 1 in that dimension.
#' 
#' All arguments to an operation with implicit expansion are coerced to arrays
#' first.
#' 
#' Currently, all arguments to an operation with implicit expansion are expanded
#' to the full size of the output first, resulting in bad performance for very large arrays.
#' 
#' The package `rray` (
#' [on GitHub](https://github.com/r-lib/rray),
#' [documentation](https://rray.r-lib.org/),
#' currently archived on [CRAN](https://CRAN.R-project.org/package=rray)
#' ) provides similar functionality as part of complete a remodeling of the array object.
#' 
#' @examples 
#' x <- c(1,2,3)
#' y <- t(c(4,5))
#' x %m+% y
#' mmapply(sum, x, x, t(x))
#' 
#' m <- matrix(3*(1:12)^2, 3, 4)
#' cm <- t(colMeans(m))
#' m %m-% cm
#' 
#' summaries <- list(Max = max, Min = min, avg = mean)
#' data <- list(a = 1:5, b = 2:3, c = 20:12)
#' formatStrings <- array(c('%.1f', '%.3f'), c(1,1,2))
#' mmapply(function(f, d, s) sprintf(s, f(d)), summaries, t(data), formatStrings)
#' 
#' @seealso 
#' [`mmapply`], [`BinaryOperators`], [`expandArray`], [`expandedDim`], [`ArrayCreation`], [`RowAndColumnVectors`]
#' 
#' @docType package
#' @name implicitExpansion-package
#' 
NULL




