#' Clear RUT
#'
#' @description Removes any character that is not a number, K, k, dot or hyphen
#' @param rut Character string
#' @return Returns a character string without said characters
#' @examples
#' clear_rut('12m345.678-9=')
#' @keywords clean
clear_rut <- function(rut) {
  rut <- gsub('[^0-9Kk|\\.&-]', '', rut)
  rut
}
