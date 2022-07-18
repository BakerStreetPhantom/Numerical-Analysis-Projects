library(plot3D)
n=100000
dt=0.01
#MATRIX FOR COORDINATES
XYZ=matrix(NA,nrow=n+1,ncol=3) 
colnames(XYZ) = c("x","y","z")
rownames(XYZ) = c(0:n)
XYZ[1,1]=10
XYZ[1,2]=0
U=matrix(NA,nrow=n+1,ncol=2)
colnames(U) = c("U","D1U")
D1XYZ = matrix(NA,nrow=n+1,ncol=2) #FIRST DERIVATIVE
D1XYZ[1,]=c(0,5)
D2XYZ = matrix(NA,nrow=n+1,ncol=2) #SECOND DERIVATIVE
for(i in 1:n){
  U[i,1] = sqrt(XYZ[i,1]^2 + XYZ[i,2]^2)
  XYZ[i,3]=sqrt(U[i,1] - 1)
  U[i,2] = ((XYZ[i,1] * D1XYZ[i,1]) + (XYZ[i,2] * D1XYZ[i,2]))/(U[i,1])
  R = (((0.5*((U[i,1]-1)^(-0.5)))*((D1XYZ[i,1]^2 + D1XYZ[i,2]^2 - U[i,2]^2)/U[i,1])) + ((U[i,2]^2)*(-0.25*((U[i,1] -1)^(-1.5)))) + 9.8)/(U[i,1] * ((0.5*((U[i,1]-1)^(-0.5))) + (2*((U[i,1]-1)^(0.5)))))
  D2XYZ[i,] = c(-XYZ[i,1]*R, -XYZ[i,2]*R)
  #2nd order Taylor's Method
  XYZ[i+1,1] = XYZ[i,1] + (dt*D1XYZ[i,1]) + ((dt^2)/2)*D2XYZ[i,1] 
  XYZ[i+1,2] = XYZ[i,2] + (dt*D1XYZ[i,2]) + ((dt^2)/2)*D2XYZ[i,2]
  #Euler's Method
  D1XYZ[i+1,1] = D1XYZ[i,1] + dt*D2XYZ[i,1] 
  D1XYZ[i+1,2] = D1XYZ[i,2] + dt*D2XYZ[i,2]
}
U[n+1,1] = (XYZ[n+1,1]^2 + XYZ[n+1,2])^0.5
XYZ[n+1,3]=(U[n+1,1] - 1)^0.5
#PLOTTING THE CURVES
plot(XYZ[,1],XYZ[,3],type="l")
lines3D(XYZ[,1],XYZ[,2],XYZ[,3],type="l")

