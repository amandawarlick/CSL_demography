

#state transition - females
getPHI <- nimbleFunction(
  run = function(z=double(0), phiP=double(0), phi1=double(0), phi2=double(0), 
                 phi3=double(0), phi4=double(0), phi5=double(0), phi6=double(0),
                 phi7=double(0), phi8=double(0), phi9=double(0), phi10=double(0), 
                 phi11=double(0), phi12=double(0), phi13=double(0), phi14=double(0),
                 phi15=double(0), phi16=double(0), phiA=double(0)) {
    returnType(double(1))
    ans <- rep(0,19)
    if(z==1)   ans <- c(0,phiP,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1-phiP)  #pup
    if(z==2)   ans <- c(0,0,phi1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1-phi1)  #yearling   
    if(z==3)   ans <- c(0,0,0,phi2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1-phi2)  #2yr
    if(z==4)   ans <- c(0,0,0,0,phi3,0,0,0,0,0,0,0,0,0,0,0,0,0,1-phi3)  #3yr 
    if(z==5)   ans <- c(0,0,0,0,0,phi4,0,0,0,0,0,0,0,0,0,0,0,0,1-phi4)  #4yr
    if(z==6)   ans <- c(0,0,0,0,0,0,phi5,0,0,0,0,0,0,0,0,0,0,0,1-phi5)  #5yr           
    if(z==7)   ans <- c(0,0,0,0,0,0,0,phi6,0,0,0,0,0,0,0,0,0,0,1-phi6)  #6yr 
    if(z==8)   ans <- c(0,0,0,0,0,0,0,0,phi7,0,0,0,0,0,0,0,0,0,1-phi7)       
    if(z==9)   ans <- c(0,0,0,0,0,0,0,0,0,phi8,0,0,0,0,0,0,0,0,1-phi8)       
    if(z==10)  ans <- c(0,0,0,0,0,0,0,0,0,0,phi9,0,0,0,0,0,0,0,1-phi9)   
    if(z==11)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,phi10,0,0,0,0,0,0,1-phi10) 
    if(z==12)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,phi11,0,0,0,0,0,1-phi11) 
    if(z==13)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,phi12,0,0,0,0,1-phi12) 
    if(z==14)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,phi13,0,0,0,1-phi13) 
    if(z==15)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,phi14,0,0,1-phi14) 
    if(z==16)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,phi15,0,1-phi15) 
    if(z==17)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,phi16,1-phi16) 
    if(z==18)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,phiA,1-phiA)       
    if(z==19)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1) #D
    
    return(ans)
  }
)

#observations 
getP <- nimbleFunction(
  run = function(z=double(0), p1=double(0), p2=double(0), p3=double(0), p4=double(0), p5=double(0),
                 p6=double(0), p7=double(0), p8=double(0), p9=double(0), 
                 p10=double(0), p11=double(0), p12=double(0), p13=double(0),
                 p14=double(0), p15=double(0), p16=double(0), 
                 pA=double(0)) {
    returnType(double(1))
    ans <- rep(0,19)
    if(z==1)   ans <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #pups seen as pups
    if(z==2)   ans <- c(0,p1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1-p1)   #1yr      
    if(z==3)   ans <- c(0,0,p2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1-p2)   #2yr
    if(z==4)   ans <- c(0,0,0,p3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1-p3)   #3yr
    if(z==5)   ans <- c(0,0,0,0,p4,0,0,0,0,0,0,0,0,0,0,0,0,0,1-p4)   #4yr 
    if(z==6)   ans <- c(0,0,0,0,0,p5,0,0,0,0,0,0,0,0,0,0,0,0,1-p5)   #5yr           
    if(z==7)   ans <- c(0,0,0,0,0,0,p6,0,0,0,0,0,0,0,0,0,0,0,1-p6)   #6yr  
    if(z==8)   ans <- c(0,0,0,0,0,0,0,p7,0,0,0,0,0,0,0,0,0,0,1-p7)   #7yr  
    if(z==9)   ans <- c(0,0,0,0,0,0,0,0,p8,0,0,0,0,0,0,0,0,0,1-p8)   #8yr  
    if(z==10)  ans <- c(0,0,0,0,0,0,0,0,0,p9,0,0,0,0,0,0,0,0,1-p9)   #9yr  
    if(z==11)  ans <- c(0,0,0,0,0,0,0,0,0,0,p10,0,0,0,0,0,0,0,1-p10)   #9yr
    if(z==12)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,p11,0,0,0,0,0,0,1-p11)   #9yr
    if(z==13)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,p12,0,0,0,0,0,1-p12)   #9yr
    if(z==14)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,p13,0,0,0,0,1-p13)   #9yr
    if(z==15)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,p14,0,0,0,1-p14)   #9yr
    if(z==16)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,p15,0,0,1-p15)   #9yr
    if(z==17)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,p16,0,1-p16)   #9yr
    if(z==18)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,pA,1-pA)   #pA
    if(z==19)  ans <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)      #nd
    
    return(ans)
  }
)
