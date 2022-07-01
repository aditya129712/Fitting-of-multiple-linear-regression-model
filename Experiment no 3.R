#R Program-
  XX=array(c(10.458,0.238,7.032,0.238,1.316,-0.268,7.032,-0.268,16.047),dim=c(3,3))
  XX
  XY=array(c(1.379,2.198,0.368),dim=c(3,1))
  XY
  XX_inv=solve(XX)
  B=(XX_inv)%*%(XY)
  B
  den=4.244
  num=den-(t(B)%*%(XX)%*%B)
  n=28
  p=3
  R_sqr=1-((num/den)*((n-1)/(n-p)))
  R_sqr
  F_cal=( R_sqr/(1- R_sqr))*((n-p-1)/p)
  F_cal
  F_tab=qf(1-0.05,3,24)
  F_tab
  EE=num
  S_sqr=EE/(n-p)
  V=S_sqr[1,1]*(XX_inv)
  V
  V_B=array(c(sqrt(V[1,1]), sqrt(V[2,2]), sqrt(V[3,3])),dim=c(3,1))
  T2=sqrt(p*qf(0.05,3,25))
  LCL=B-(V_B*T2)
  LCL
  UCL= B+(V_B*T2)
  UCL
  #We write the following program to get the pair (LCL, UCL) for Beta1, Beta2 and Beta3
  CONF_INTRVL=mat.or.vec(3,2)
  for (i in 1:3){
    CONF_INTRVL[i,1]=c(LCL[i])
    CONF_INTRVL[i,2]=c(UCL[i])}
  CONF_INTRVL
  #Result from the R-Programming-
  #F_cal=57.7116
  #F_tab=3.008787
  #THE VALUE OF REGRESSION COEFFICIENTS ARE-
  #Beta1=0.08521572
  #Beta2=1.65750420
  #Beta3=0.01327190
  #AND CONFIDENCE INTERVAL FOR Beta1, Beta2 AND Beta3 IS GIVEN BY-
  #  0.05499460 	0.11543684
  #  1.58583258 	1.72917582
  #  -0.01111644	 0.03766024
  
  