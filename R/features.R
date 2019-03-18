#' Extract features from a dataset
#'
#' @param .data A dataset (tsibble)
#' @param ... Additional arguments to be passed to measures that use it.
#'
#' @export
features <- function(.data, ...){
  UseMethod("features")
}

combine_features <- function(...){
  dots <- dots_list(...)
  res <- map2(dots, names(dots), function(val, nm){
    if(nchar(nm) > 0){
      nm <- paste0(nm, "_")
    }
    set_names(as.list(val), paste0(nm,names(val)))
  })
  as_tibble(flatten(unname(res)))
}

build_feature_calls <- function(measures, available_args){
  # Build measure calls
  missing_args <- chr()
  fns <- map(measures, function(fn){
    args <- formals(fn)
    args <- args[names(args) != "..."]
    req_args <- names(args)[map_lgl(args, is_missing)]
    if(!all(match_req <- req_args %in% available_args)){
      missing_args <<- c(missing_args, req_args[!match_req])
      return(NULL)
    }

    # Function call
    inputs <- available_args[available_args%in%names(args)]
    call2(fn, !!!set_names(syms(inputs), inputs))
  })

  if(!is_empty(missing_args)){
    warn(
      sprintf("Could not estimate all measures as the following arguments are missing: %s",
              paste0(missing_args, collapse = ", "))
    )
  }

  # names(fns) <- names(fns) %||% seq_along(fns)
  fns
}

#' @export
features.tbl_ts <- function(.data, y = NULL, ..., features = list(guerrero)){
  dots <- dots_list(...)

  if(is_function(features)){
    features <- list(features)
  }

  if(quo_is_null(enquo(y))){
    inform(sprintf(
      "Feature variable not specified, automatically selected `y = %s`",
      measured_vars(.data)[1]
    ))
    y <- sym(measured_vars(.data)[1])
  }
  else{
    y <- enexpr(y)
  }

  features <- squash(features)

  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, .data, .auto = "smallest")
  }

  .data <- transmute(.data, x = !!y)

  fns <- build_feature_calls(features, c(names(dots), "x"))

  with(dots,
       .data %>%
         as_tibble %>%
         group_by(!!!key(.data), !!!groups(.data)) %>%
         summarise(
           .features = list(combine_features(!!!compact(fns)))
         ) %>%
         unnest(.features) %>%
         ungroup()
  )
}

#' @inherit tsfeatures::crossing_points
#' @importFrom stats median
#' @export
crossing_points <- function(x)
{
  midline <- median(x, na.rm = TRUE)
  ab <- x <= midline
  lenx <- length(x)
  p1 <- ab[1:(lenx - 1)]
  p2 <- ab[2:lenx]
  cross <- (p1 & !p2) | (p2 & !p1)
  c(crossing_points = sum(cross, na.rm = TRUE))
}

#' @inherit tsfeatures::arch_stat
#' @importFrom stats lm embed
#' @export
arch_stat <- function(x, lags = 12, demean = TRUE)
{
  if (length(x) <= 13) {
    return(c(arch_lm = NA_real_))
  }
  if (demean) {
    x <- x - mean(x, na.rm = TRUE)
  }
  mat <- embed(x^2, lags + 1)
  fit <- try(lm(mat[, 1] ~ mat[, -1]), silent = TRUE)
  if ("try-error" %in% class(fit)) {
    return(c(arch_lm = NA_real_))
  }
  arch.lm <- summary(fit)
  c(arch_lm = arch.lm$r.squared)
}
