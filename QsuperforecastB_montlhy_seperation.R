source('C:/Users/korin/Downloads/P3_Korina2.R')
P1=7.3896
P2=0.9853

Emax=24*(P1+P2)

#C:\Users\korin\Documents\NTUA\NTUA\Diplomatiki\excel\achelos\After EGU\the chosen ones

data=as.data.frame(Uncertainty_each_month)

DE_month_metric_two=matrix(NA,nrow =14474,ncol=12 )
months=as.matrix(data[,3])

for (i in 1:14474) {
  for (j in 1:12){
    
    if(months[i]==j){
      
      DE_month_metric_two[i,j]=data[i,4]
    }
    else{
      
      DE_month_metric_two[i,j]=NA
      
    }
    
  }
}



#Statistical characteristics of each month

oct_DEfor=c(na.omit(DE_month_metric_two[,10]))
nov_DEfor=c(na.omit(DE_month_metric_two[,11]))
dec_DEfor=c(na.omit(DE_month_metric_two[,12]))
jan_DEfor=c(na.omit(DE_month_metric_two[,1]))
feb_DEfor=c(na.omit(DE_month_metric_two[,2]))
mar_DEfor=c(na.omit(DE_month_metric_two[,3]))
apr_DEfor=c(na.omit(DE_month_metric_two[,4]))
may_DEfor=c(na.omit(DE_month_metric_two[,5]))
jun_DEfor=c(na.omit(DE_month_metric_two[,6]))
jul_DEfor=c(na.omit(DE_month_metric_two[,7]))
aug_DEfor=c(na.omit(DE_month_metric_two[,8]))
sept_DEfor=c(na.omit(DE_month_metric_two[,9]))


DEfor_metric2_months=matrix(NA, ncol=12, nrow=1240)
DEfor_metric2_months[1:length(jan_DEfor), 1]=jan_DEfor
DEfor_metric2_months[1:length(feb_DEfor), 2]=feb_DEfor
DEfor_metric2_months[1:length(mar_DEfor), 3]=mar_DEfor
DEfor_metric2_months[1:length(apr_DEfor), 4]=apr_DEfor
DEfor_metric2_months[1:length(may_DEfor), 5]=may_DEfor
DEfor_metric2_months[1:length(jun_DEfor), 6]=jun_DEfor
DEfor_metric2_months[1:length(jul_DEfor), 7]=jul_DEfor
DEfor_metric2_months[1:length(aug_DEfor), 8]=aug_DEfor
DEfor_metric2_months[1:length(sept_DEfor), 9]=sept_DEfor
DEfor_metric2_months[1:length(oct_DEfor), 10]=oct_DEfor
DEfor_metric2_months[1:length(nov_DEfor), 11]=nov_DEfor
DEfor_metric2_months[1:length(dec_DEfor), 12]=dec_DEfor

colnames(DEfor_metric2_months)=month.abb
head(DEfor_metric2_months)

write.table(x = DEfor_metric2_months, file = 'DEfor_metric2_months.csv', sep = ';')



write.csv(DEfor_metric2_months,'DEfor_metric2_months.csv',row.names=FALSE)
write.csv(aug_DEfor,'aug.csv',row.names=FALSE)


average_jan=mean(jan_DEfor)
average_feb=mean(feb_DEfor)
average_mar=mean(mar_DEfor)
average_apr=mean(apr_DEfor)
average_may=mean(may_DEfor)
average_jun=mean(jun_DEfor)
average_jul=mean(jul_DEfor)
average_aug=mean(aug_DEfor)
average_sept=mean(sept_DEfor)
average_oct=mean(oct_DEfor)
average_nov=mean(nov_DEfor)
average_dec=mean(dec_DEfor)



average_data=c(average_jan,average_feb,average_mar,average_apr,average_may,average_jun,average_jul,average_aug,average_sept,average_oct,average_nov,average_dec)


correl_jan=cor(jan_DEfor[1:1208],jan_DEfor[2:1209])
correl_feb=cor(feb_DEfor[1:1101],feb_DEfor[2:1102])
correl_mar=cor(mar_DEfor[1:1208],mar_DEfor[2:1209])
correl_apr=cor(apr_DEfor[1:1169],apr_DEfor[2:1170])
correl_may=cor(may_DEfor[1:1223],may_DEfor[2:1224])
correl_jun=cor(jun_DEfor[1:1199],jun_DEfor[2:1200])
correl_jul=cor(jul_DEfor[1:1239],jul_DEfor[2:1240])
correl_aug=cor(aug_DEfor[1:1239],aug_DEfor[2:1240])
correl_sept=cor(sept_DEfor[1:1199],sept_DEfor[2:1200])
correl_oct=cor(oct_DEfor[1:1239],oct_DEfor[2:1240])
correl_nov=cor(nov_DEfor[1:1199],nov_DEfor[2:1200])
correl_dec=cor(dec_DEfor[1:1239],dec_DEfor[2:1240])



correl_data=c(correl_jan,correl_feb,correl_mar,correl_apr,correl_may,correl_jun,correl_jul,correl_aug,correl_sept,correl_oct,correl_nov,correl_dec)



stdev_jan=sd(jan_DEfor)
stdev_feb=sd(feb_DEfor)
stdev_mar=sd(mar_DEfor)
stdev_apr=sd(apr_DEfor)
stdev_may=sd(may_DEfor)
stdev_jun=sd(jun_DEfor)
stdev_jul=sd(jul_DEfor)
stdev_aug=sd(aug_DEfor)
stdev_sept=sd(sept_DEfor)
stdev_oct=sd(oct_DEfor)
stdev_nov=sd(nov_DEfor)
stdev_dec=sd(dec_DEfor)




stdev_data=c(stdev_jan,stdev_feb,stdev_mar,stdev_apr,stdev_may,stdev_jun,stdev_jul,stdev_aug,stdev_sept,stdev_oct,stdev_nov,stdev_dec)

library(moments)

skewness_jan=skewness(jan_DEfor)
skewness_feb=skewness(feb_DEfor)
skewness_mar=skewness(mar_DEfor)
skewness_apr=skewness(apr_DEfor)
skewness_may=skewness(may_DEfor)
skewness_jun=skewness(jun_DEfor)
skewness_jul=skewness(jul_DEfor)
skewness_aug=skewness(aug_DEfor)
skewness_sept=skewness(sept_DEfor)
skewness_oct=skewness(oct_DEfor)
skewness_nov=skewness(nov_DEfor)
skewness_dec=skewness(dec_DEfor)


skewness_data=c(skewness_jan,skewness_feb,skewness_mar,skewness_apr,skewness_may,skewness_jun,skewness_jul,skewness_aug,skewness_sept,skewness_oct,skewness_nov,skewness_dec)

Statistics_of_eah_month_final=data.frame(average_data,correl_data,stdev_data,skewness_data)

Statistics_of_eah_month_final_matrix=as.matrix(Statistics_of_eah_month_final)

Statistics_of_eah_month_final=data.frame(Statistics_of_eah_month_final)
write.csv(Statistics_of_eah_month_final,'statistics_of_months.csv')

#write.csv(Statistics_of_eah_month_final,'Statistics_of_each_month_metric_two.csv')

#gamma distribution of white noise
#parameters

params=list()

for (i in 1:12) {  
  params[[i]]=P3params(m =Statistics_of_eah_month_final_matrix[i,1],
                       s = Statistics_of_eah_month_final_matrix[i,3],
                       Csk =Statistics_of_eah_month_final_matrix[i,4] )
  }


params_data_frame=as.data.frame(params)
params_matrix=matrix(params_data_frame,nrow=3,ncol=12,byrow=FALSE)

# params=P3params(m =Statistics_of_eah_month_final_matrix[i,1],s = Statistics_of_eah_month_final_matrix[i,3],Csk =Statistics_of_eah_month_final_matrix[i,4] )
MEAN=matrix(NA,ncol=1,nrow=12)
STDEV=matrix(NA,ncol=1,nrow=12)
SKEW=matrix(NA,ncol=1,nrow=12)

#STATISTICS CHARACTERISTICS OF EACH MONTH - PEARSON 3
for ( i in 1:12) {
  
  x=rp3(n=1447400,shape=params[[i]]$k,location = params[[i]]$ci,
        
  scale = params[[i]]$l, csk = Statistics_of_eah_month_final_matrix[i,4])
  
  MEAN[i,1]=mean(x)
  STDEV[i,1]=sd(x)
  SKEW[i,1]=moments::skewness(x)
  
}




#write.csv(x[1:10^6,],'random_sample1_Jan.csv',row.names=FALSE)
#write.csv(x[11^6:1447400,],'random_sample2_Jan.csv',row.names=FALSE)
# the added error in our forecast


shape_DEfor=params_matrix[1,]
scale_DEfor=params_matrix[2,]
DEfor_gam=matrix(NA,ncol=100,nrow=14474,byrow =FALSE)
mean_st=matrix(NA,ncol=1,nrow=14474)
stdev_st=matrix(NA,ncol=1,nrow=14474)


for (i in 1: 14474 ){
  
  M=data[i,3]
 
  for (j in 1:100){

    
    DEfor_gam[i,j]=rp3(n=1,shape=params[[M]]$k,location = params[[M]]$ci,
                       scale = params[[M]]$l, 
                       csk = Statistics_of_eah_month_final_matrix[M,4])
    }
}

write.csv(DEfor_gam,'DEfor_gam-m.csv',row.names=FALSE)



energyfinal=matrix(NA,nrow=14474,ncol=100)

for (i in 1:100) {
  for (j in 1:14474) {
    
    energyfinal[j,i]=max(0,min(Emax,data[j,5]+DEfor_gam[j,i]))
    
  }
  
}
energy_with_uncertainty_sample=energyfinal
write.csv(energy_with_uncertainty_sample,'energy_metric_twofinal_sample-m.csv')



energylarge=c()
energylow=c()
energymedian=c()
for (i in 1:14474){
  
  #load Rfast package
  
  energylarge[i]=nth(energyfinal[i,],10,descending=TRUE)
  energylow[i]=nth(energyfinal[i,],10,descending =FALSE )
  energymedian[i]=nth(energyfinal[i,],50,descending =FALSE )}

write.csv(energylarge,'energy_large-m.csv',row.names =FALSE )
write.csv(energylow,'energy_low-m.csv',row.names =FALSE)
write.csv(energymedian,'energy_median-m.csv',row.names =FALSE)
days=data[,1]


