#' RUT format: Dots and Hyphen (Puntos y guion)
#'
#' @description Changes the format of a RUT to dots and hyphen
#' @param rut RUT to modify. It must be a character string
#' @param uppercase Defines if the verification number 'k' should be in uppercase (TRUE) or lowercase (FALSE). By default the value is FALSE
#' @return Returns a RUT in dots and hyphen format
#' @examples
#' rut_complete('123456789')
#' @keywords format
rut_complete <- function(rut, uppercase = FALSE) {
  if( class(rut) != 'character' ) {
    stop('El rut ingresado no es una cadena de texto')
  }

  rut <- as.character(rut)
  if( uppercase ) {
    rut <- toupper(rut)
  } else {
    rut <- tolower(rut)
  }

  rut <- gsub('\\.|-', '', rut)
  rut <- rev(unlist(strsplit(rut, "")))

  rut <- rev(R.utils::insert(rut, ats = c(2, 5, 8), values = c('-', '.', '.')))

  rut <- paste(rut, collapse = "")
  rut
}


#' RUT format: Hyphen without dots (Sin puntos, con guion)
#'
#' @description Changes the format of a RUT to hyphen without dots
#' @param rut RUT to modify. It must be a character string
#' @param uppercase Defines if the verification number 'k' should be in uppercase (TRUE) or lowercase (FALSE). By default the value is FALSE
#' @return Returns a RUT in hyphen without dots format
#' @examples
#' rut_hyphen('123456789')
#' @keywords format
rut_hyphen <- function(rut, uppercase = FALSE) {
  if( class(rut) != 'character' ) {
    stop('El rut ingresado no es una cadena de texto')
  }

  rut <- as.character(rut)
  if( uppercase ) {
    rut <- toupper(rut)
  } else {
    rut <- tolower(rut)
  }

  rut <- gsub('\\.|-', '', rut)
  rut <- rev(unlist(strsplit(rut, "")))

  rut <- rev(R.utils::insert(rut, ats = c(2), values = c('-')))

  rut <- paste(rut, collapse = "")
  rut
}


#' RUT format: Without dots and Hyphen (Sin puntos ni guion)
#'
#' @description Changes the format of a RUT to not contain dots or hyphen
#' @param rut RUT to modify. It must be a character string
#' @param uppercase Defines if the verification number 'k' should be in uppercase (TRUE) or lowercase (FALSE). By default the value is FALSE
#' @return Returns a RUT whitout dots and hyphen
#' @examples
#' rut_simple('123456789')
#' @keywords format
rut_simple <- function(rut, uppercase = FALSE) {
  if( class(rut) != 'character' ) {
    stop('El rut ingresado no es una cadena de texto')
  }

  rut <- as.character(rut)
  if( uppercase ) {
    rut <- toupper(rut)
  } else {
    rut <- tolower(rut)
  }

  rut <- gsub('\\.|-', '', rut)
  rut
}
