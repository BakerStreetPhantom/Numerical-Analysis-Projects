#CREATING M11
M11<-matrix(c(0),nrow=2401,ncol=2401)
for(i in 1:2401)
{ 
  for(j in 1:2401)
  { 
    if(i==j)
    { 
      M11[i,j]=20000;
    }
    else if(abs(i-j)==1)
    {
      M11[i,j]=-5000;
    }       
    else if(abs(i-j)==49)
    {
      M11[i,j]=-5000;
    }
    else M11[i,j]=0;
  }
}
for(i in 1:48)
{
  M11[49*i,49*i+1]=0;
  M11[49*i+1,49*i]=0;
}

#CREATING M12
M12<-matrix(c(0),nrow=2401,ncol=200)
for(i in 1:49)
{ 
  M12[49*i,51+i]=-2*50*50;
  M12[49*48+i,151-i]=-2*50*50;
  M12[1+49*(i-1),200-(i-1)]=-2*50*50;
  for(j in 2:50)
  { 
    if((j-i)==1)
    {
      M12[i,j]=-2*50*50;
    }
    
  }
}

#CREATING C2
C2=c(0);
for(i in 1:50)
{
  C2[i]=0;
  C2[50+i]=(i-1)/50;
  C2[100+i]=1;
  C2[150+i]=((51-i)/50)^3;
}
#FINDING C1
C1=solve(M11,-M12%*%C2);

#PLOTTING THE SHAPE
X=seq(0,1,len=51);
Y=seq(0,1,len=51);
#Creating Z=f(X,Y)
Z<-matrix(nrow=51,ncol=51)
Z[1,1]=C2[151];
Z[51,1]=C2[151];
Z[1,51]=C2[1];
Z[51,51]=C2[51];
for(i in 2:50)
{ 
  Z[i,1]=C2[152-i];
  Z[i,51]=C2[i];
  Z[1,i]=C2[150+i];
  Z[51,i]=C2[102-i];
  for(j in 2:50)
  {
    Z[i,j]=C1[49*(50-j)+i-1];
  }
}
library(rgl)
persp3d(X,Y,Z,col='springgreen')