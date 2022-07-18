data <- read.table("https://arnabc74.github.io/numana/data.txt",header = FALSE);
xdata=c(0)
for(i in 1:6)
{
  for(j in 1:166)
  xdata[(i-1)*166+j]=data[j,i]
}
xbar=mean(xdata);
logxdata=c(0);
for(i in 1:966){logxdata[i]=log(xdata[i])}
logxbar=mean(logxdata)
new.ral <- function(x){
  y <- (digamma(x)-log(x)+log(xbar)-logxbar)/(trigamma(x)-(1/x));
}
p=c();
p[1]=0.5
for(i in 2:50){p[i]=p[i-1] - new.ral(p[i-1])}
P=p[50];
A=P/xbar
hist(xdata)
x=seq(0,3,len=61)
plot(dgamma(x,shape=P,scale=1/A),type='h')