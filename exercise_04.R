rm(list = ls())
mydata<-read.csv("percep_data.csv")
g_x<-mydata[1:1000,1]
g_y<-mydata[1:1000,2]
group<-mydata[1:1000,3]
N=1000

plot(g_x, g_y, type='n', xlab='X', ylab='Y')
for(i in 1:N){
  if(group[i]==1){
    points(g_x[i],g_y[i],col='red')
  }else{
    points(g_x[i],g_y[i],col='blue')
  }
}

θ0=0.1
θ1=0.2
θ2=0.3

M=15
eta=0.005
th=0.9
verbose=F

for(i in 1:M){
  print(paste('Epoch starts',i))
  index=1:N
  for(j in index){
    y_j=θ0+θ1*g_x[j]+θ2*g_y[j]
  if(y_j>=0){
    pred_j=1
  }else{
    pred_j=-1
  }
    θ0 = θ0 + eta*(group[j] - pred_j)*1.0
    
    θ1 = θ1 + eta*(group[j] - pred_j)*g_x[j]
    
    θ2 = θ2 + eta*(group[j] - pred_j)*g_y[j]
    
  if (verbose == T){
    
    print(paste('  -> updating data point ', j, ' : '))
    
    print(paste('     -> θ0: ' ,θ0))
    
    print(paste('     -> θ0: ' ,θ1))
    
    print(paste('     -> θ0: ' ,θ2))
    
  }
}
y_all = θ0 + θ1*g_x + θ2*g_y

y_pred = y_all

y_pred[y_all >= 0] = 1

y_pred[y_all< 0] = -1



acc = sum(y_pred == group)/length(group)

print(paste('Epoch ends: ', i, ' WITH accuracy: ', acc))

if (acc >= th){
  
  break
  
}

}
y_all = θ0 + θ1*g_x + θ2*g_y

print(y_all)



y_pred = y_all

y_pred[y_all >= 0] = 1

y_pred[y_all< 0] = -1



print(y_pred)



acc = sum(y_pred == group)/length(group)

print(acc)

plot(g_x, g_y, type='n', xlab='X', ylab='Y')

for(i in 1:N){
  if(group[i]==1){
    points(g_x[i],g_y[i],col='red')
  }else{
    points(g_x[i],g_y[i],col='blue')
  }
}


abline(a = -1.0*θ0/θ2, b = -1.0*θ1/θ2, col='dark green', lwd=3, lty=2)
print(paste(θ0,'+',θ1,'x1+',θ2,'x2','=0'))
