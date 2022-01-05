#import super duper excel file  model for rain's data and dataset excel


data=as.data.frame(dataset)
#gross 
H<-150 
 #Calculating the characteristics of Turbine 1 
#power T1(selected)
 # in MW 

 P1<-7.4
nmin<-0.33
 nmin1<-0.33
 nmax1<-0.93
 
#shape_parameters
 a1<-0.78
 b1<-3.11
 
 
#% of qmin
theta<-0.15
#max and min of qturb1(m3/sec)
qmax1<-1000*P1/(9.81*H*nmax1*0.95)
qmin1<-theta*qmax1


 #Calculating the characteristics of Turbine 2 
 #power T2 (selected in MW)
 P2<-1
 nmin2<-0.33
 nmax2<-0.93
 
#shape_parameters
a2<-0.78
b2<-3.11

#max and min of qturb2(m3/sec)

qmax2<-1000*P2/(9.81*H*nmax2*0.95)
qmin2<-theta*qmax2

#maximum energy produced MWh
Emax=(P1+P2)*24

#number of rows of  data
nts<-nrow(data)
Qturb<-c()
Qturb_1<-c()
Pturb_1<-c()
Eturb_1<-c()
Qturb_2<-c()
Pturb_2<-c()
Eturb_2<-c()
Etotal_turb<-c()
n1<-c()
n2<-c()
Enextday=c()
Enextday_forecast=c()



for (i in 1:nts){
  
  #operational rule appliance 
  
  Qturb[i]=min(data[i,3],qmax1+qmax2)
  
  if (Qturb[i] >= qmin2){
    
    if ( Qturb[i]<=1.275){
      Qturb_1[i]=0
      n1[i]=0
      Qturb_2[i]=min(qmax2,Qturb[i])
      
      n2[i]<-nmin2+(1-(1-(((Qturb_2[i]/qmax2)-theta)/(1-theta))^a2)^b2)*(nmax2-nmin2)
    }
    
    
    else if (1.275<Qturb[i]&& Qturb[i]<=5.675){
      
      Qturb_2[i]=0
      n2[i]=0
      Qturb_1[i]=min(qmax1,Qturb[i])
      
      n1[i]=nmin1+(1-(1-(((Qturb_1[i]/qmax1)-theta)/(1-theta))^a1)^b1)*(nmax1-nmin1)
      
      
    }
    
    
    else if( Qturb[i]>5.675){
      
      
      Qturb_2[i]=qmax2
      n2[i]<-nmin2+(1-(1-(((Qturb_2[i]/qmax2)-theta)/(1-theta))^a2)^b2)*(nmax2-nmin2)
      Qturb_1[i]=Qturb[i]-Qturb_2[i]
      
      
      
      n1[i]=nmin1+(1-(1-(((Qturb_1[i]/qmax1)-theta)/(1-theta))^a1)^b1)*(nmax1-nmin1)
      
    }
    
    
    
    
    
    
    
    
    Pturb_1[i]<-Qturb_1[i]*9.81*H*n1[i]*0.95/1000
    
    Eturb_1[i]<-Pturb_1[i]*24
    
    
    Pturb_2[i]<-Qturb_2[i]*9.81*H*n2[i]*0.95/1000
    
    Eturb_2[i]<-Pturb_2[i]*24
    #Calculating total energy produced by both turbines in MWh
    Etotal_turb[i]<-Eturb_1[i]+Eturb_2[i] 
    
    
  }
  
  else {
    
    Qturb_1[i]=0
    Qturb_2[i]=0
    n1[i]=0
    n2[i]=0
    Eturb_1[i]=0 
    Eturb_2[i]=0
    Pturb_1[i]=0
    Pturb_2[i]=0
    Etotal_turb[i]=0
  }
}





  
Etotal_turb_dataframe=as.data.frame(Etotal_turb)

write.csv(Etotal_turb,row.names=FALSE,'Etotalturb_crossroad.csv')

save(Etotal_turb_dataframe,file="Eforecastingturb.RData")



#Calculation of next day's energy from historic data
  for ( k in 2:nts){ 
    Enextday[k]=Etotal_turb[k-1]
    
  }
Enextday_dataframe=as.data.frame(Enextday)
write.csv(Etotal_turb_dataframe,row.names=FALSE,'Etotalturb_generic.csv')

save(Enextday_dataframe,file='Enextdayfromhistoric.RData')


#forecasting [model 

#parameters
A=4.787
k=0.062
n=0.476
y=0.124
z=0.152



# forecasting energy 

data_rain=as.data.frame(SUPER_DUPER)

for ( m in 3:nts) { if ( data_rain[m-1,3]>0.1){
  Enextday_forecast[m]=min(Emax,A*((Etotal_turb[m-1])^n)*((data[m-1,3])^k)*((Etotal_turb[m-2])^y)*(data_rain[m-1,3]^z))
}
  else {Enextday_forecast[m]= Etotal_turb[m-1]}
  
}
write.csv(as.matrix(Enextday_forecast,ncol=1),row.names=FALSE,'Eforecasted_generic.csv')

#Sum of mean square errors

DE=Etotal_turb-Enextday
DE_square=DE^2
DEforecast=Etotal_turb-Enextday_forecast

write.csv(as.matrix(DEforecast,ncol=1),row.names=FALSE,'DEforecasted_generic.csv')

DEforecast_square=DEforecast^2

Sum_DE_square_error=sum(DE_square,na.rm =TRUE )
Sum_DEforecast_square_error=sum(DEforecast_square,na.rm =TRUE )



EFF=1-(Sum_DEforecast_square_error/Sum_DE_square_error)
x=1:nts
y=2:nts


plot(x,Etotal_turb,type='l',col='green',xlab='days',ylab='Energy (GWh)')

lines(y,Enextday_forcast,col='blue')

legend(2,1,c('energy produced according to historic data','next day energy according to experimental model'),lwd=c(5,2),col=c('green','blue'),y.intersp=1.5)
data1=as.matrix(data)
plot(data1[,3],Etotal_turb)

#AR1 model
#Defor statistics

mean_DEfor=mean(DEforecast,na.rm=TRUE)
stdev_DEfor=sd(DEforecast,na.rm=TRUE)

library(moments)


skewness_DEfor=skewness(DEforecast,na.rm=TRUE)
#calculate autocorrelation with lag 1

xt_0=c(DEforecast[1:14476])

xt_1=c(DEforecast[2:14477])



correlation_lag1=cor(xt_0,xt_1,method="pearson",use="complete.obs")
#gamma distribution of white noise
#parameters
stdev_whitenoise=((stdev_DEfor^2)*(1-correlation_lag1^2))^0.5

mean_whitenoise=mean_DEfor*(1-correlation_lag1)

Csv_whitenoise=skewness_DEfor*(1-correlation_lag1^3)/(1-correlation_lag1^2)^1.5

shape_DEfor=4/Csv_whitenoise^2

rate_DEfor=(shape_DEfor^0.5)/stdev_whitenoise

DEfor_gam=rgamma(1447900,shape=shape_DEfor,scale=1/rate_DEfor)
  
Defor_gam_matrix=matrix(DEfor_gam,nrow = 14479,ncol=100,byrow =TRUE)


energyfinal=matrix(rep(0),nrow=14479,ncol=100)
for (i in 1:100) {
  for (j in 6:14479) {
    energyfinal[j,i]=min(200,Etotal_turb_for2[j]+Defor_gam_matrix[j,i])
    
  }
  
}

energyfinal_sample=energyfinal
write.csv(energyfinal_sample,'energy_generic_sample.csv')