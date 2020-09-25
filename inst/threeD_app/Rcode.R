spruce.df = read.csv("SPRUCE.csv")

myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}

rsq = function(xk,data){ # data=spruce.df
  df=within(data, X<-(BHDiameter-xk)*(BHDiameter>xk))
  lmp=lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}

rsqdash = function(xk,h,data) {
 (rsq((xk+h/2),data)-rsq((xk-h/2),data))/h
}


myf2 = function(x,xk,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}

coeff = function(xk,xk2,data){ # data=spruce.df
  df=within(data, {
            X<-(BHDiameter-xk)*(BHDiameter>xk)
            X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
            )
  lmp=lm(Height ~ BHDiameter + X + X2, data=df)
  coef(lmp)
}


# The function that finds the max r^2 in the confidence interval using grid approximation
matrix_rsquared = function(x1, x2, h, data)
{
  max_r = 0
  xk1 = 0
  xk2 = 0
  values = c()
  for(x_1 in seq(from=x1[1], to=x1[2], by=h)){
    for(x_2 in seq(from=x2[1], to=x2[2], by=h)){
      r_2 = rsq_xk1_xk2(x_1, x_2, data)

      if(r_2 > max_r)
      {
        max_r = r_2
        xk1 = x_1
        xk2 = x_2
      }
    }
  }
  a = c(xk1, xk2, max_r)
  values = append(values, a)
  values
}

rsq_xk1_xk2_data = function(xk1, xk2){
  sp2.df=within(spruce.df, {
    X<-(BHDiameter-xk1)*(BHDiameter>xk1)
    X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  )

  lmp = lm(Height ~ BHDiameter + X + X2, data = sp2.df)
  tmp=summary(lmp) # tmp holds the summary info
  tmp$r.squared
}





