

A2018_level1=0.1/100
year1=c(2018:2040)
t1=year1-2018
h1=seq(from =0.5,by=-0.01,length.out=length(t1))
r1=seq(from=0.7,by=0.1,length.out=length(t1))
A1=h1/(h1+r1)+(A2018_level1-h1/(h1+r1))*exp(-(h1+r1)*t1)
A1=cbind(year1,A1)
plot(A1,type='l')


A2018_level2=0.01/100
year2=c(2018:2040)
t2=year2-2018
h2=seq(from =0.5,by=-0.01,length.out=length(t2))
r2=seq(from=0.7,by=0.05,length.out=length(t2))
A2=h2/(h2+r2)+(A2018_level2-h2/(h2+r2))*exp(-(h2+r2)*t2)
A2=cbind(year2,A2)
plot(A2,type='l')



A2020_level3=0.01/100
year3=c(2020:2040)
t3=year3-2020
h3=seq(from =0.6,by=-0.01,length.out=length(t3))
r3=seq(from=0.9,by=0.8,length.out=length(t3))
A3=h3/(h3+r3)+(A2020_level3-h3/(h3+r3))*exp(-(h3+r3)*t3)
A3=cbind(year3,A3)
plot(A3,type='l')

A2025_level4=0.001/100
year4=c(2025:2040)
t4=year4-2025
h4=seq(from =0.3,by=-0.01,length.out=length(t4))
r4=seq(from=0.2,by=0.01,length.out=length(t4))
A4=h4/(h4+r4)+(A2025_level4-h4/(h4+r4))*exp(-(h4+r4)*t4)
A4=cbind(year4,A4)
plot(A4,type='l')

A2025_level5=0.001/100
year5=c(2025:2040)
t5=year5-2025
h5=seq(from =0.4,by=-0.01,length.out=length(t5))
r5=seq(from=0.1,by=0.01,length.out=length(t5))
A5=h5/(h5+r5)+(A2025_level5-h5/(h5+r5))*exp(-(h5+r5)*t5)
A5=cbind(year5,A5)

A2018_level0=0.99
year0=c(2018:2040)
t0=year0-2018
h0=seq(from =0.3,by=-0.001,length.out=length(t0))
r0=seq(from=0.7,by=0.3,length.out=length(t0))
A0=h0/(h0+r0)+(A2018_level0-h0/(h0+r0))*exp(-(h0+r0)*t0)
A0=cbind(year0,A0)
plot(A0,type='l')

plot(A1,type='l',col=1,ylim=c(0,1),xlab="year",ylab="proportion",main="Prediction of autonomous vehicles proportion")
lines(A2,type='l',col=2)
lines(A3,type='l',col=3)
lines(A4,type='l',col=4)
lines(A5,type='l',col=5)
lines(A0,type='l',col=6)
legend(2020,0.95,legend=c("level0","level1", "level2","level3", "level4","level5"),col=c(6,1:5), lty=1, cex=0.8)
