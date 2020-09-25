#' coeff
#'
#'Creates a linear model using the x knots and data. The function then returns the coefficients of the linear model
#'
#' @param xk the first x knot
#' @param xk2 the second k knot
#' @param data the data for the linear model
#'
#' @return coefficients of the linear model
#' @export
#'
#' @examples
#' coeff(xk,xk2,data)
coeff = function(xk,xk2,data){ # data=spruce.df
  df=within(data, {
    X<-(BHDiameter-xk)*(BHDiameter>xk)
    X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  )
  lmp=lm(Height ~ BHDiameter + X + X2, data=df)
  coef(lmp)
}
