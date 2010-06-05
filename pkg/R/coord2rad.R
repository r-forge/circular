coord2rad <- function(x, y=NULL, control.circular=list()) {
  if (NCOL(x)!=2) {
    stop('a matrix or a dataframe with 2 columns is required if y is NULL')
    x <- atan2(x[,2],x[,1])
  } else if (is.null(y)) {
    stop('x must have two columns if y is NULL')
  } else {
    x <- as.vector(x)
    y <- as.vector(y)
    x <- atan2(y,x)
  }
  datacircularp <- list(type="angles", units="radians", template="none", modulo="2pi", zero=0, rotation="counter")  
  dc <- control.circular
  if (is.null(dc$type))
    dc$type <- datacircularp$type
  if (is.null(dc$units))
    dc$units <- datacircularp$units
  if (is.null(dc$template))
    dc$template <- datacircularp$template
  if (is.null(dc$modulo))
    dc$modulo <- datacircularp$modulo
  if (is.null(dc$zero))
    dc$zero <- datacircularp$zero
  if (is.null(dc$rotation))
    dc$rotation <- datacircularp$rotation
  x <- conversion.circular(circular(x), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  return(x)
}
