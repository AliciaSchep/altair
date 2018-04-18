#' @export
pype <- function(pyobj, pymethod, ...){

  pyobj[[as.character(substitute(pymethod))]](...)

}
