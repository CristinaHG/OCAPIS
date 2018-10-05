#' rscala bridge name
#'
#' Object from rscala package, it returns the name of the bridge between r and Scala
#'
#' @return The Name of the bridge between R and Scala. By default is 's'
"rscalaBridgeName"


#' rscala bridge owner
#'
#' Object from rscala package, is a boolean pointing if the embeded scala bridge in the package has owner
#'
#' @return TRUE if embeded scala bridge in the package has owner, else FALSE
"rscalaBridgeOwner"


#' Scala interpreter instance
#'
#' Scala interpreter instance named 's' by default. Name can be changed using 'assign.name'
#'
#' @param ... scala code snipped of function to call from R
#' @return The avaiable Scala instance
"s"

