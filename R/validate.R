#' Calculate verification numer of a RUT
#'
#' @param num RUT to calculate
#' @return Expected verification number
#' @keywords internal
#' @examples
#' calculate_dv("12345678")
calculate_dv <- function(num) {
  suma <- 0
  multiplo <- 2
  rut <- rev(unlist(strsplit(num, "")))

  for( i in rut ) {
    index <- multiplo * as.numeric(i)

    suma <- suma + index

    if( multiplo < 7 ) {
      multiplo <- multiplo + 1
    } else {
      multiplo = 2
    }

  }

  dvEsperado <- 11 - (suma %% 11)

  if( dvEsperado == 10 ) {
    return ('k')
  } else if ( dvEsperado == 11 ) {
    return ("0")
  } else {
    return ( as.character(dvEsperado) )
  }

}


#' Validate RUT
#'
#' @description Validates a Chilean RUT. The input must be a character string in one of the following formats: RUT with dots and hyphen (Ex. '12.345.678-9'), RUT without dots but with hyphen (Ex. '12345678-9') or RUT without dots and hyphen (Ex. '123456789')
#' @param rut Character str
#' @return Returs TRUE if RUT is valid, else it will return FALSE
#' @examples
#' validate_rut('123456789')
#' validate_rut('12345678-9')
#' validate_rut('12.345.678-9')
#' @keywords validate
validate_rut <- function(rut) {
  if( class(rut) != 'character' ) {
    stop('El rut ingresado no es una cadena de texto')
  }

  # Estandarizar RUTs: remueve puntos y guiones. Convierte RUTs terminados en K a minuscula
  rut <- tolower( gsub('\\.|-', '', rut) )

  # Checkear que solo queden numeros o la letra k
  rgxTest <- grepl('^[0-9k]*$', rut)
  if( !rgxTest ) {
    return (FALSE)
  }

  # Separar RUT de digito verificador
  num <- substr(rut, 1, nchar(rut) - 1)
  dgv <- substr(rut, nchar(rut), nchar(rut))

  # Verificar que el RUT es mayor a 1.000.000
  if( as.numeric(num) < 1000000 ) {
    return (FALSE)
  }

  # Calcular dgvEsperado
  dgvCalc <- calculate_dv(num)

  # Verificar RUT
  if( dgvCalc == dgv ) {
    return (TRUE)
  } else {
    return (FALSE)
  }

}
