df <- data.frame("a" = c(1,2,3,4,5), "b" = c(1,2,3,3,4))
df[df$b == 3] = 6
print(df)
df
df$date = c("08-jan-2020","09-jan-2020","10-mar-2020","11-feb-2020","05-jan-2020")
month = substr(df$date,4,6)
month[month == 'jan'] = 1
month[month == 'feb'] = 2
month[month == 'mar'] = 3
print(month)

ks_score<-function(target,pred,d=20){
  
  set.seed(8)
  
  rand=runif(length(pred))
  
  rank=order(pred,rand,decreasing=TRUE)
  
  s=length(pred)
  
  ds=s%/%d
  
  rank_ord=0
  
  for (i in 1:d)
    
  {
    
    rank_ord[((i-1)*ds+1):(i*ds)]=i
    
  }
  
  rank_ord[(d*ds):s]=d
  
  perf=data.frame(pred=pred)
  
  perf$rank=1:s
  
  perf$rank[rank]=rank_ord
  
  y=table(perf$rank,target)
  
  ks=data.frame(ne=y[,1],e=y[,2])
  
  ks$eperc=100*ks$e/sum(target==1)
  
  ks$neperc=100*ks$ne/sum(target==0)
  
  ks$ceperc[1]=ks$eperc[1]
  
  ks$cneperc[1]=ks$neperc[1]
  
  for (i in 2:d)
    
  {
    
    ks$ceperc[i]=ks$eperc[i]+ks$ceperc[i-1]
    
    ks$cneperc[i]=ks$neperc[i]+ks$cneperc[i-1]
    
  }
  
  ks$KS=ks$ceperc-ks$cneperc
  
  for (i in 1:(d-1))
    
  {
    
    ks$lift[i]=ks$ceperc[i]/((100/d)*i)
    
  }
  
  return(ks)
  
}
