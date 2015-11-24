##  Function that takes a linear regression model and allows
## ggplot to place the equation text (and r^2) onto the graph
## itself
## ....from stackoverflow somewhere.

#for this one, m is a linear model:
#  m <- lm(y~x, df)
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*", "~~italic(r)^2~"="~r2,l)
    
  }
   else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)

  }
   as.character(as.expression(eq));
  #r <- as.character(as.expression(eq2))
  
  #sprintf("%s \n %s", eqn, r );
  

}

## then add to ggplot with: 
##  + annotate("#text", x = 25, y = 300, label = 
##      lm_eqn(lm(y ~ x, df)), 
##      colour="black", size = 5, parse=TRUE)

