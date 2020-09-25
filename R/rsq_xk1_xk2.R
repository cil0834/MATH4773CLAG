#' rsq_xk1_xk2
#'
#' @param xk1 x knot 1
#' @param xk2 x knot 2
#' @param data the data used in the linear model
#' This function returns the r^2 value of the linear model given x knot 1 and x knot 2
#'
#' @return the r^2 value of the linear model given the data and the x knots
#' @export
#'
#' @examples
#' rsq_xk1_xk2(xk1, xk2, spruce.df)
rsq_xk1_xk2 = function(xk1, xk2, data){
  sp2.df=within(data, {
    X<-(BHDiameter-xk1)*(BHDiameter>xk1)
    X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  )

  lmp = lm(Height ~ BHDiameter + X + X2, data = sp2.df)
  tmp=summary(lmp) # tmp holds the summary info
  tmp$r.squared
}
