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
l=1:10
l2=l^2
class(l2)
library(sloop)
otype(l2)
ftype(print) 
x=1:100
s3_methods_class("median")
s3_get_method(median.default)

s3_dispatch(median(x))


x=1:40;

set.seed(30); y=2*x + 10 + rnorm(40,0,5)

Then

ylm = lm(y~x)
summary.default
f = table(rep(LETTERS[1:5],c(3,4,2,5,3)))
class(f)
