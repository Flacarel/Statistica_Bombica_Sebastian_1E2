outliers_mean=function(file){
  sample<-scan(file)
  m = mean(sample)
  s = sd(sample)
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] <m - 2*s | sample[i] > m + 2*s){
      j = j + 1
      outliers[j] = sample[i]
    }
  return(outliers)
}

outliers_iqr=function(file){
  sample<-scan(file)
  q1=as.vector(quantile(sample))[2]
  q3=as.vector(quantile(sample))[4]
  iqr=q3-q1
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] <q1 - 1.5*iqr | sample[i] > q3 + 1.5*iqr){
      j = j + 1
      outliers[j] = sample[i]
    }
  return(outliers)
}




