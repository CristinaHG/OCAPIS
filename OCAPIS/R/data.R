#' Train Data Partition of Balace-scale ordinal set
#'
#' This data set was generated to model psychological experimental results. Each example is classified as
#' having the balance scale tip to the right, tip to the left, or be balanced. The attributes are the left weight,
#' the left distance, the right weight, and the right distance. The correct way to find the class is the greater of
#' (left-distance * left-weight) and (right-distance * right-weight). If they are equal, it is balanced.
#'
#' @format A data set with 468 instances and 5 variables:
#'
#' \describe{
#'   \item{Left-Weight}{5 (1, 2, 3, 4, 5) }
#'   \item{Left-Distance}{5 (1, 2, 3, 4, 5) }
#'   \item{Right-Weight}{ 5 (1, 2, 3, 4, 5)}
#'   \item{Right-Distance}{5 (1, 2, 3, 4, 5)}
#'   \item{Class}{3 (1, 2, 3) }
#' }
#'
#' @source \href{http://www.uco.es/grupos/ayrna/orreview}{AYRNA}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml/datasets/Balance+Scale}{UCI ML Repository}.
"dattrain"

#' Test Data Partition of Balace-scale ordinal set
#'
#' This data set was generated to model psychological experimental results. Each example is classified as
#' having the balance scale tip to the right, tip to the left, or be balanced. The attributes are the left weight,
#' the left distance, the right weight, and the right distance. The correct way to find the class is the greater of
#' (left-distance * left-weight) and (right-distance * right-weight). If they are equal, it is balanced.
#'
#' @format A data set with 157 instances and 5 variables:
#'
#' \describe{
#'   \item{Left-Weight}{5 (1, 2, 3, 4, 5) }
#'   \item{Left-Distance}{5 (1, 2, 3, 4, 5) }
#'   \item{Right-Weight}{ 5 (1, 2, 3, 4, 5)}
#'   \item{Right-Distance}{5 (1, 2, 3, 4, 5)}
#'   \item{Class}{3 (1, 2, 3) }
#' }
#'
#' @source \href{http://www.uco.es/grupos/ayrna/orreview}{AYRNA}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml/datasets/Balance+Scale}{UCI ML Repository}.
"dattest"
