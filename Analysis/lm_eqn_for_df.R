##  Function that takes a linear regression model and allows
## ggplot to place the equation text (and r^2) onto the graph
## itself
## ....from stackoverflow somewhere.


lm_eqn = function(df){
  m = lm(y ~ x, df);
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*"," ~~italic(r)^2~"="~r2,l)
  } else {
    b = b*(-1)
    eq <- substitute(italic(y) == a - b %.% italic(x)*"," ~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

#Use like this (if p is ggplot)
#p1 = p + geom_text(aes(x = 25, y = 300, label = lm_eqn(df)), parse = TRUE)