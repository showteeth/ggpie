.onLoad <- function(libname, pkgname) {
  # set global variables in order to avoid CHECK notes
  utils::globalVariables("Add")
  utils::globalVariables("CumFreq")
  utils::globalVariables("CumSum")
  utils::globalVariables("Freq")
  utils::globalVariables("Median")
  utils::globalVariables("Num")
  utils::globalVariables("angle")
  utils::globalVariables("element_id")
  utils::globalVariables("element_type")
  utils::globalVariables("fill")
  utils::globalVariables("group")
  utils::globalVariables("label")
  utils::globalVariables("vorder")
  utils::globalVariables("x")
  utils::globalVariables("y")
  utils::globalVariables("zorder")
  utils::globalVariables("Sum")
  utils::globalVariables("hjust")

  invisible()
}
