Xbar<-2
Xcv<-3
Xsigma<-sqrt(log(Xcv^2+1))
Xmu<-log(Xbar)-(Xsigma^2)/2
X<-rlnorm(1E6,Xmu,Xsigma)
mean(X)
sd(X)/mean(X)

dlnorm(1,0,Xsigma)
dlnorm(1,0,Xsigma*1.5)