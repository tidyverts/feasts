add_class <- function(x, new_class){
  `class<-`(x, union(new_class, class(x)))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

require_package <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    abort(
      sprintf('The `%s` package must be installed to use this functionality. It can be installed with install.packages("%s")', pkg, pkg)
    )
  }
}

check_gaps <- function(x){
  if (any(has_gaps(x)$.gaps)) {
    abort(sprintf("%s contains implicit gaps in time. You should check your data and convert implicit gaps into explicit missing values using `tsibble::fill_gaps()` if required.", deparse(substitute(x))))
  }
}

`%empty%` <- function(x, y) {
  if (length(x) == 0) y else x
}

lag <- function(x, n){
  if (n == 0)
    return(x)
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(rep(NA, n), x[seq_len(xlen - n)])
  out
}
