data_Q=as.data.frame(SUPER_DUPER)

Qinflow=as.vector(data_Q,mode='any')

#xt_0=c(Qinflow[1:14478,5])
#xt_1=c(Qinflow[2:14479,5])


#correlation_lag1=cor(xt_0,xt_1,method="pearson",use="complete.obs")

#QAR1=c()


#for (i in 2 :14479){
  #QAR1[i]=correlation_lag1*Qinflow[i-1,5]+(1-correlation_lag1)*Qinflow[i-1,6]
#}

#QAR1_dataframe=as.data.frame(QAR1,na.rm=TRUE)

Qrain_model=c()


#parameters
a1=0.449
a2=0.386
b=0.069
c=0.159
d1=0.381
d2=0.511
b2=0.064








for ( i in 6 :14479){
  if (Qinflow[i-1,3]>0.1){ Qrain_model[i]=a1*Qinflow[i,4]+a2*Qinflow[i-1,5]+b*Qinflow[i,6]+c*Qinflow[i-1,3]
  
  }
  else { Qrain_model[i]=d1*Qinflow[i,4]+d2*Qinflow[i-1,5]+b2*Qinflow[i,6]
  }
}
Qrain_model_dataframe=as.data.frame(Qrain_model,na.rm=TRUE)

#save(QAR1_dataframe,file='QAR1_DATA.RData')
save(Qrain_model_dataframe,file='Qrain_model_DATA.RData')