#constructor

# Make some data

x = 1:30
y = 4 + 8*x + rnorm(30,0,10)

windows(); plot(y~x, bg = "blue", cex =2, pch =21)

# This example is a little artificial but lets you see how the S3 OOP is working

# Constructor

myconstr = function(x, y){
  ylm = lm(y~x)
  obj = list(data = list(x = x, y = y), ylm = ylm)
  class(obj) = "constr"
  obj
}

z = myconstr(x,y)
class(z)


plot.constr = function(x, pch=21,bg="Blue", cex =3){
  plot(x[["data"]][["x"]],x[["data"]][["y"]], 
       pch = pch, 
       bg = bg,
       xlab = "x",
       ylab = "y"
       )
  abline(x$ylm, lwd = 2, col = "Red")
}

windows();plot(z)

print.constr = function(x){
  summary(x$ylm)
}
print(z)
