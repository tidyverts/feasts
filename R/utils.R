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

unnest_tbl <- function(.data, tbl_col, .sep = NULL){
  row_indices <- rep.int(seq_len(NROW(.data)), map_int(.data[[tbl_col[[1]]]], NROW))

  nested_cols <- map(tbl_col, function(x){
    lst_col <- .data[[x]]
    if(is.data.frame(lst_col[[1]])){
      dplyr::bind_rows(!!!set_names(lst_col, rep(x, length(lst_col))))
    }
    else{
      list2(!!x := unlist(lst_col))
    }
  })

  if(!is.null(.sep)){
    nested_cols <- map2(
      nested_cols, tbl_col,
      function(x, nm) set_names(x, paste(nm, colnames(x), sep = .sep))
    )
  }

  dplyr::bind_cols(
    .data[row_indices, setdiff(names(.data), tbl_col)], # Parent cols
    !!!nested_cols # Nested cols
  )
}

unnest_tsbl <- function(.data, tsbl_col, parent_key = NULL, interval = NULL){
  tsbl <- .data[[tsbl_col]][[1L]]
  if (!is_tsibble(tsbl)) {
    abort("Unnested column is not a tsibble object.")
  }
  idx <- index(tsbl)
  idx_chr <- as_string(idx)
  key <- c(parent_key, key_vars(tsbl))

  .data <- unnest_tbl(.data, tsbl_col)

  class(.data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(.data, key = !!key, index = !!idx,
                index2 = !!index2(tsbl), ordered = is_ordered(tsbl),
                interval = interval%||%interval(tsbl))
}
