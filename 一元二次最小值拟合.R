#generate x,y
x=seq(-5,5,0.01)

y=function(x){2*x^2+3*x+5}
grad=function(x){4*x+3}
plot(x,y(x),type='l')
lines(c(-3/4,-3/4),c(0,70),col='red',lty=2)
text(-0.5,60,"Closedform solution",col="red",pos=4)

#initial assume
x0=5;stepFactor=.1;c=1e-3;max_n=500;j=1
xtrace=c(x0);ytrace=c(y(x0))

for (i in 1:max_n){
  tempx=xtrace[j]-stepFactor*grad(xtrace[j])
  d=y(tempx)-y(xtrace[j])
  print(paste(i,tempx,d))
  if(d>=0 & grad(xtrace[j])!=0){stepFactor=stepFactor/2;next}
  xtrace=c(xtrace,tempx);j=j+1
  ytrace=c(ytrace,y(tempx))
  if(-c<d & d<=0) break
}
print(paste('x=',xtrace[length(xtrace)],', y=',ytrace[length(xtrace)],sep = ''))
plot(x,y(x),type='l')
lines(c(-3/4,-3/4),c(0,70),col='red',lty=2)
text(-0.5,60,"Closedform solution",col="red",pos=4)
lines(xtrace,ytrace,col='blue',type='b')
