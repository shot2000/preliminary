
rm(list = ls(all = TRUE)) # remove all data
# read data
Reg <- read.dbf('E:/01-Dissertation/02_Preliminary_Research/04-Regression_Analysis/Reg.dbf')

# CHange Column names
names(Reg )[6]<-paste("AT")
names(Reg )[8]<-paste("TOTAL_HH")
names(Reg )[9]<-paste("AVGWK") 
names(Reg )[10]<-paste("AVGPER")
names(Reg )[11]<-paste("AVGAUTO")
names(Reg )[19]<-paste("EMP_L") 
names(Reg )[20]<-paste("EMP_M") 
names(Reg )[21]<-paste("EMP_H") 
names(Reg )[23]<-paste("POP") 

# Linear model
lm1 = lm(CarEMTAZ ~ ACRES + AT + POP + TOTAL_HH+TOTAL_EMPL+POP_DENSIT+EMP_DENSIT+TOTAL_AUTO+EMP_L+EMP_M+EMP_H +AVGWK+AVGPER+ AVGAUTO+Avg_CarbEM+Avg_TRIPDI+Avg_TRIPSP+Avg_TRIPDU, data=Reg)

summary (lm1)

Call:
lm(formula = CarEMTAZ ~ ACRES + AT + POP + TOTAL_HH + TOTAL_EMPL + 
    POP_DENSIT + EMP_DENSIT + TOTAL_AUTO + EMP_L + EMP_M + EMP_H + 
    AVGWK + AVGPER + AVGAUTO + Avg_CarbEM + Avg_TRIPDI + Avg_TRIPSP + 
    Avg_TRIPDU, data = Reg)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.04382 -0.25622 -0.03848  0.21543  2.67275 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.458e-01  2.392e-01  -0.609   0.5426    
ACRES        8.637e-05  4.723e-05   1.829   0.0679 .  
AT           1.902e-01  4.779e-02   3.979 7.66e-05 ***
POP          4.898e-04  1.076e-04   4.551 6.34e-06 ***
TOTAL_HH     3.751e-04  2.253e-04   1.665   0.0963 .  
TOTAL_EMPL  -3.716e-05  2.521e-05  -1.474   0.1409    
POP_DENSIT  -2.093e-02  4.931e-03  -4.244 2.50e-05 ***
EMP_DENSIT   3.927e-04  2.867e-04   1.370   0.1712    
TOTAL_AUTO   4.782e-04  1.067e-04   4.483 8.65e-06 ***
EMP_L       -1.394e-02  2.371e-01  -0.059   0.9531    
EMP_M        2.460e-01  2.305e-01   1.067   0.2862    
EMP_H        2.668e-02  2.378e-01   0.112   0.9107    
AVGWK        2.038e-01  1.027e-01   1.983   0.0477 *  
AVGPER      -3.420e-02  4.291e-02  -0.797   0.4257    
AVGAUTO     -2.011e-01  7.606e-02  -2.643   0.0084 ** 
Avg_CarbEM  -6.731e+01  6.595e+01  -1.021   0.3078    
Avg_TRIPDI  -1.769e-03  2.245e-02  -0.079   0.9372    
Avg_TRIPSP   2.041e-02  3.937e-03   5.184 2.87e-07 ***
Avg_TRIPDU  -2.636e-05  1.587e-04  -0.166   0.8681    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 0.5016 on 674 degrees of freedom
Multiple R-squared: 0.8005,	Adjusted R-squared: 0.7951 
F-statistic: 150.2 on 18 and 674 DF,  p-value: < 2.2e-16 

##VIF > 10 is highly colinear
vif (lm1)
     ACRES         AT        POP   TOTAL_HH TOTAL_EMPL POP_DENSIT EMP_DENSIT 
  1.736518   2.754549  28.302553  23.558846   1.511926   1.889728   1.644586 
TOTAL_AUTO      EMP_L      EMP_M      EMP_H      AVGWK     AVGPER    AVGAUTO 
 14.982419   9.948009  11.756604   7.800208   3.737032   2.662386   5.087788 
Avg_CarbEM Avg_TRIPDI Avg_TRIPSP Avg_TRIPDU 
 34.084902  19.409570   4.914306  13.207564 

# Linear model #2 after dropping colineared variables
lm2 = lm(CarEMTAZ ~ ACRES + AT + +TOTAL_EMPL+POP_DENSIT+EMP_DENSIT+EMP_L+EMP_H +AVGWK+AVGPER+ AVGAUTO++Avg_TRIPSP, data=Reg)

summary (lm2)

Call:
lm(formula = CarEMTAZ ~ ACRES + AT + +TOTAL_EMPL + POP_DENSIT + 
    EMP_DENSIT + EMP_L + EMP_H + AVGWK + AVGPER + AVGAUTO + +Avg_TRIPSP, 
    data = Reg)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.3528 -0.5588 -0.1209  0.4192  3.0672 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.340e-01  1.628e-01  -0.823 0.410543    
ACRES        2.546e-04  7.907e-05   3.219 0.001346 ** 
AT           1.662e-01  8.092e-02   2.054 0.040366 *  
TOTAL_EMPL  -7.908e-05  4.286e-05  -1.845 0.065447 .  
POP_DENSIT   4.401e-02  7.546e-03   5.832 8.47e-09 ***
EMP_DENSIT  -3.310e-04  4.773e-04  -0.693 0.488242    
EMP_L       -5.192e-01  1.441e-01  -3.603 0.000337 ***
EMP_H       -4.351e-01  1.597e-01  -2.726 0.006584 ** 
AVGWK        3.845e-01  1.753e-01   2.193 0.028663 *  
AVGPER       6.233e-02  6.920e-02   0.901 0.368033    
AVGAUTO      3.422e-02  1.143e-01   0.299 0.764840    
Avg_TRIPSP   3.794e-02  3.522e-03  10.773  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.8619 on 681 degrees of freedom
Multiple R-squared:  0.4048,	Adjusted R-squared:  0.3952 
F-statistic: 42.11 on 11 and 681 DF,  p-value: < 2.2e-16



# Stepwise Regression / variable selection
lm3<- step(lm2, direction="backward")
Start:  AIC=-194.07
CarEMTAZ ~ ACRES + AT + +TOTAL_EMPL + POP_DENSIT + EMP_DENSIT + 
    EMP_L + EMP_H + AVGWK + AVGPER + AVGAUTO + +Avg_TRIPSP

             Df Sum of Sq    RSS     AIC
- AVGAUTO     1     0.067 505.97 -195.98
- EMP_DENSIT  1     0.357 506.26 -195.58
- AVGPER      1     0.603 506.51 -195.25
<none>                    505.91 -194.07
- TOTAL_EMPL  1     2.529 508.44 -192.62
- AT          1     3.134 509.04 -191.79
- AVGWK       1     3.572 509.48 -191.19
- EMP_H       1     5.519 511.43 -188.55
- ACRES       1     7.699 513.61 -185.60
- EMP_L       1     9.644 515.55 -182.98
- POP_DENSIT  1    25.266 531.17 -162.30
- Avg_TRIPSP  1    86.216 592.12  -87.02

Step:  AIC=-195.98
CarEMTAZ ~ ACRES + AT + TOTAL_EMPL + POP_DENSIT + EMP_DENSIT + 
    EMP_L + EMP_H + AVGWK + AVGPER + Avg_TRIPSP

             Df Sum of Sq    RSS      AIC
- EMP_DENSIT  1     0.361 506.33 -197.486
- AVGPER      1     0.554 506.53 -197.222
<none>                    505.97 -195.980
- TOTAL_EMPL  1     2.587 508.56 -194.446
- AT          1     4.528 510.50 -191.805
- EMP_H       1     5.479 511.45 -190.516
- AVGWK       1     6.601 512.57 -188.998
- ACRES       1     7.972 513.95 -187.146
- EMP_L       1     9.789 515.76 -184.701
- POP_DENSIT  1    25.592 531.57 -163.786
- Avg_TRIPSP  1    87.704 593.68  -87.202

Step:  AIC=-197.49
CarEMTAZ ~ ACRES + AT + TOTAL_EMPL + POP_DENSIT + EMP_L + EMP_H + 
    AVGWK + AVGPER + Avg_TRIPSP

             Df Sum of Sq    RSS      AIC
- AVGPER      1     0.641 506.98 -198.610
<none>                    506.33 -197.486
- TOTAL_EMPL  1     4.100 510.43 -193.897
- EMP_H       1     5.331 511.67 -192.228
- AT          1     6.133 512.47 -191.143
- AVGWK       1     6.438 512.77 -190.730
- ACRES       1     7.668 514.00 -189.070
- EMP_L       1     9.501 515.84 -186.603
- POP_DENSIT  1    26.395 532.73 -164.271
- Avg_TRIPSP  1    88.122 594.46  -88.295

Step:  AIC=-198.61
CarEMTAZ ~ ACRES + AT + TOTAL_EMPL + POP_DENSIT + EMP_L + EMP_H + 
    AVGWK + Avg_TRIPSP

             Df Sum of Sq    RSS      AIC
<none>                    506.98 -198.610
- TOTAL_EMPL  1     4.339 511.31 -194.704
- EMP_H       1     5.697 512.67 -192.865
- AT          1     6.886 513.86 -191.260
- ACRES       1     8.088 515.06 -189.641
- EMP_L       1     9.488 516.46 -187.760
- AVGWK       1    14.305 521.28 -181.326
- POP_DENSIT  1    29.323 536.30 -161.644
- Avg_TRIPSP  1    90.393 597.37  -86.908

#####################################################

#The final model
lm4 = lm(CarEMTAZ ~ ACRES + AT + TOTAL_EMPL + POP_DENSIT + EMP_L + EMP_H + 
    AVGWK + Avg_TRIPSP, data=Reg)

summary (lm4) 

Call:
lm(formula = CarEMTAZ ~ ACRES + AT + TOTAL_EMPL + POP_DENSIT + 
    EMP_L + EMP_H + AVGWK + Avg_TRIPSP, data = Reg)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.3796 -0.5451 -0.1164  0.4226  3.0828 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.269e-01  1.511e-01  -0.840 0.401472    
ACRES        2.561e-04  7.754e-05   3.303 0.001005 ** 
AT           2.036e-01  6.678e-02   3.048 0.002392 ** 
TOTAL_EMPL  -9.433e-05  3.899e-05  -2.419 0.015805 *  
POP_DENSIT   4.550e-02  7.234e-03   6.290 5.67e-10 ***
EMP_L       -5.110e-01  1.428e-01  -3.578 0.000371 ***
EMP_H       -4.393e-01  1.585e-01  -2.773 0.005714 ** 
AVGWK        4.883e-01  1.111e-01   4.393 1.29e-05 ***
Avg_TRIPSP   3.844e-02  3.481e-03  11.043  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.8609 on 684 degrees of freedom
Multiple R-squared:  0.4036,	Adjusted R-squared:  0.3966 
F-statistic: 57.86 on 8 and 684 DF,  p-value: < 2.2e-16

 #Regression Diagnostics    
 # visualize four graphs at once
par(family = 'Helvetica')
par(mfrow=c(2,2))
plot(lm4, col="blue",pch=20)

###the final model
lmf = lm(CarEMTAZ ~ ACRES + AT + TOTAL_EMPL + POP_DENSIT + EMP_L + EMP_H + 
    AVGWK + Avg_TRIPSP, data=Reg)

 #Extracting Elements of the Model Object
 
 Coe<- lmf[[1]]                          # extract list item 1: coefficients
 
 Res<- lmf[[2]]                         # extract list item 2: residuals


###################################################

## Model Cross-Validation

library(DAAG)
cv.lm(df=Reg, lmf, m=9) # 9 fold cross-validation
cv.lm(df = Reg, lmf, m=9, dots = 
      FALSE, seed=29, plotit=TRUE, printit=TRUE)
	  

# not so good fit 
lm3 = lm(CarEMTAZ ~ ACRES +TOTAL_EMPL+EMP_DENSIT+X_ZONE_NUMB+EMP_L+EMP_M+EMP_H +AVGWK+AVGPER+ AVGAUTO+Sum_TRIPDU+Avg_TRIPDI+Avg_TRIPDU, data=Reg)
summary (lm3)

Call:
lm(formula = CarEMTAZ ~ ACRES + TOTAL_EMPL + EMP_DENSIT + X_ZONE_NUMB + 
    EMP_L + EMP_M + EMP_H + AVGWK + AVGPER + AVGAUTO + Sum_TRIPDU + 
    Avg_TRIPDI + Avg_TRIPDU, data = Reg)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.8479 -0.5752 -0.1307  0.4339  3.2626 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -5.443e-01  4.103e-01  -1.327  0.18507    
ACRES        2.067e-04  7.644e-05   2.704  0.00702 ** 
TOTAL_EMPL  -1.042e-04  4.301e-05  -2.422  0.01570 *  
EMP_DENSIT  -6.263e-04  4.610e-04  -1.358  0.17476    
X_ZONE_NUMB  2.438e-04  1.599e-04   1.524  0.12790    
EMP_L        1.036e-01  4.110e-01   0.252  0.80114    
EMP_M        7.524e-01  3.992e-01   1.885  0.05986 .  
EMP_H        2.222e-01  4.115e-01   0.540  0.58934    
AVGWK        3.228e-01  1.761e-01   1.833  0.06726 .  
AVGPER       1.808e-01  6.850e-02   2.640  0.00848 ** 
AVGAUTO      4.455e-02  9.798e-02   0.455  0.64949    
Sum_TRIPDU   1.731e-04  2.201e-05   7.862 1.49e-14 ***
Avg_TRIPDI  -4.202e-02  2.016e-02  -2.084  0.03752 *  
Avg_TRIPDU   7.746e-04  1.850e-04   4.187 3.19e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 0.8693 on 679 degrees of freedom
Multiple R-squared: 0.3964,	Adjusted R-squared: 0.3848 
F-statistic:  34.3 on 13 and 679 DF,  p-value: < 2.2e-16 

par(family = 'Helvetica')
par(mfrow=c(2,2))
plot(lm3, col="blue",pch=20)