#' myf2
#'
#'Takes in data and x knot values to create a linear equation.
#'
#' @param x points on the curve
#' @param xk the first x knot
#' @param xk2 the second k knot
#' @param coef linear model coefficients
#'
#' @return coefficients of the linear model used for plotting linear equations
#' @export
#'
#' @examples
#' myf2(x,xk=input$xk1,xk2 = input$xk2, coef=coef(lmp)
myf2 = function(x,xk,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}
