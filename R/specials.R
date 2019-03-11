#' @importFrom stats model.frame
model_xreg <- function(...){
  model_formula <- new_formula(
    lhs = NULL,
    rhs = reduce(enexprs(...), function(.x, .y) call2("+", .x, .y))
  )
  model.frame(model_formula, data = self$data, na.action = stats::na.pass)
}
