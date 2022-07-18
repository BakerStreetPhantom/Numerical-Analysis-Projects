n=1500                   
# X,Y Coordinates
XY=matrix(NA,nrow=n+1,ncol=2)
colnames(XY)=c("x","y")          
rownames(XY)=c(0:n)
# Velocity along X and Y
Vel=matrix(NA,nrow=n+1,ncol=2)
colnames(Vel)=c("vx","vy")
# Acceleration along X and Y
Acc=matrix(NA,nrow=n,ncol=2)
colnames(Acc)=c("ax","ay")         
XY[1,]=c(1,-2)                   
Vel[1,]=c(0,0)                    
#CONSTANTS
l=4                               
m=1                                
k=1
t=0.01                            
g=9.80665             
for(i in 1:n){
  Acc[i,1] = (-k * XY[i,1] *(sqrt((XY[i,1])^2 + (XY[i,2])^2) - l)) / (m * (sqrt(XY[i,1]^2 + XY[i,2]^2)))
  Acc[i,2] = ((-k * XY[i,2] * (sqrt((XY[i,1])^2 + (XY[i,2])^2) - l)) / (m * (sqrt(XY[i,1]^2 + XY[i,2]^2)))) - g
  Vel[i+1,] = Vel[i,] + t*Acc[i,]
  XY[i+1,]= XY[i,] + t*Vel[i,]
}
plot(XY[,1],XY[,2])

