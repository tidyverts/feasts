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
