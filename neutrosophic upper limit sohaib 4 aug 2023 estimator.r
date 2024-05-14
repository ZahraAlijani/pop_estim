library(MASS)
set.seed(100) 
N=1000; N
n=100; n

a=8
b=(1.2)^2
y1=rnorm(1000,a,b)
c=18
d=(0.7)^2
x1=y1+rnorm(1000,c,d)
#z<-mvrnorm(N,mux2,sigz);

# RANKING VARIABLE
r1<- rank(x1)


ybar=mean(y1);ybar;
xbar=mean(x1);xbar;
rbar=mean(r1);rbar;



lm=(1/n-1/N);lm;


rhoyx=cor(y1,x1);rhoyx;
rhoyr=cor(y1,r1);rhoyr;
rhoxr=cor(x1,r1);rhoxr;
cy=sd(y)/ybar;cy;
cx=sd(x)/xbar;cx;
cr=sd(r)/rbar;cr;



######srs#####
vy1=lm*ybar^2*cy^2;vy1;
### Ratio and product #####
mse2=lm*ybar^2*(cy^2+cx^2-2*rhoyx*cy*cx);mse2;
mse3=lm*ybar^2*(cy^2+cx^2+2*rhoyx*cy*cx);mse3;
###### reg ######
mse4=lm*ybar^2*cy^2*(1-rhoyx^2);mse4;
######## BT RATIO and product####
mse5=lm*ybar^2*(cy^2+(1/4)*cx^2-rhoyx*cy*cx);mse5;
mse6=lm*ybar^2*(cy^2+(1/4)*cx^2+rhoyx*cy*cx);mse6;
############## Singh######
library(moments)
kurtosis(x)
a=1;a;
b=cx;b;
tt1=((a*xbar)/(a*xbar+b));tt1;
singh1=(lm*ybar^2/4)*(4*cy^2+tt1^2*cx^2-4*tt1*rhoyx*cy*cx);singh1;
######
a=1;a;
b=kurtosis(x);b;
tt10=((a*xbar)/(a*xbar+b));tt10;
singh2=(lm*ybar^2/4)*(4*cy^2+tt10^2*cx^2-4*tt10*rhoyx*cy*cx);singh2;
#############
a=kurtosis(x);a;
b=cx;b;
tt2=((a*xbar)/(a*xbar+b));tt2;
singh3=(lm*ybar^2/4)*(4*cy^2+tt2^2*cx^2-4*tt2*rhoyx*cy*cx);singh3;
###############
a=cx;a;
b=kurtosis(x);b;
tt3=((a*xbar)/(a*xbar+b));tt3;
singh4=(lm*ybar^2/4)*(4*cy^2+tt3^2*cx^2-4*tt3*rhoyx*cy*cx);singh4;
##################
a=1;a;
b=rhoyx;b;
tt4=((a*xbar)/(a*xbar+b));tt4;
singh5=(lm*ybar^2/4)*(4*cy^2+tt4^2*cx^2-4*tt4*rhoyx*cy*cx);singh5;
###############
a=cx;a;
b=rhoyx;b;
tt5=((a*xbar)/(a*xbar+b));tt5;
singh6=(lm*ybar^2/4)*(4*cy^2+tt5^2*cx^2-4*tt5*rhoyx*cy*cx);singh6;
#####################
a=rhoyx;a;
b=cx;b;
tt6=((a*xbar)/(a*xbar+b));tt6;
singh7=(lm*ybar^2/4)*(4*cy^2+tt6^2*cx^2-4*tt6*rhoyx*cy*cx);singh7;
######################
a=kurtosis(x);a;
b=rhoyx;b;
tt7=((a*xbar)/(a*xbar+b));tt7;
singh8=(lm*ybar^2/4)*(4*cy^2+tt7^2*cx^2-4*tt7*rhoyx*cy*cx);singh8;
#########################
a=rhoyx;a;
b=kurtosis(x);b;
tt8=((a*xbar)/(a*xbar+b));tt8;
singh9=(lm*ybar^2/4)*(4*cy^2+tt8^2*cx^2-4*tt8*rhoyx*cy*cx);singh9;
################
a=1;a;
b=N*xbar1;b;
tt9=((a*xbar)/(a*xbar+b));tt9;
singh10=(lm*ybar^2/4)*(4*cy^2+tt9^2*cx^2-4*tt9*rhoyx*cy*cx);singh10;
############################## proposed  estr###
library(moments)
kurtosis(x)
a=1;a;
b=cx;b;
tt1=((a*xbar)/(a*xbar+b));tt1;
qm1=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt1^4*cx^4)-16*lm*tt1^2*cx^2*cy^2*(1-rhoyx^2));qm1;
qn1=64*(1+lm*cy^2*(1-rhoyx^2));qn1;
gk1=qm1/qn1;gk1;
######################
a=1;a;
b=kurtosis(x);b;
tt10=((a*xbar)/(a*xbar+b));tt10;
qm10=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt10^4*cx^4)-16*lm*tt10^2*cx^2*cy^2*(1-rhoyx^2));qm10;
qn10=64*(1+lm*cy^2*(1-rhoyx^2));qn10;
gk2=qm10/qn10;gk2;
####################
a=kurtosis(x);a;
b=cx;b;
tt2=((a*xbar)/(a*xbar+b));tt2;
qm2=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt2^4*cx^4)-16*lm*tt2^2*cx^2*cy^2*(1-rhoyx^2));qm2;
qn2=64*(1+lm*cy^2*(1-rhoyx^2));qn2;
gk3=qm2/qn2;gk3;
###################
a=cx;a;
b=kurtosis(x);b;
tt3=((a*xbar)/(a*xbar+b));tt3;
qm3=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt3^4*cx^4)-16*lm*tt3^2*cx^2*cy^2*(1-rhoyx^2));qm3;
qn3=64*(1+lm*cy^2*(1-rhoyx^2));qn3;
gk4=qm3/qn3;gk4;
#########################3
a=1;a;
b=rhoyx;b;
tt4=((a*xbar)/(a*xbar+b));tt4;
qm4=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt4^4*cx^4)-16*lm*tt4^2*cx^2*cy^2*(1-rhoyx^2));qm4;
qn4=64*(1+lm*cy^2*(1-rhoyx^2));qn4;
gk5=qm4/qn4;gk5;
################################
a=cx;a;
b=rhoyx;b;
tt5=((a*xbar)/(a*xbar+b));tt5;
qm5=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt5^4*cx^4)-16*lm*tt5^2*cx^2*cy^2*(1-rhoyx^2));qm5;
qn5=64*(1+lm*cy^2*(1-rhoyx^2));qn5;
gk6=qm5/qn5;gk6;
##########################
a=rhoyx;a;
b=cx;b;
tt6=((a*xbar)/(a*xbar+b));tt6;
qm6=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt6^4*cx^4)-16*lm*tt6^2*cx^2*cy^2*(1-rhoyx^2));qm6;
qn6=64*(1+lm*cy^2*(1-rhoyx^2));qn6;
gk7=qm6/qn6;gk7;
############################
a=kurtosis(x);a;
b=rhoyx;b;
tt7=((a*xbar)/(a*xbar+b));tt7;
qm7=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt7^4*cx^4)-16*lm*tt7^2*cx^2*cy^2*(1-rhoyx^2));qm7;
qn7=64*(1+lm*cy^2*(1-rhoyx^2));qn7;
gk8=qm7/qn7;gk8;
#############################
a=rhoyx;a;
b=kurtosis(x);b;
tt8=((a*xbar)/(a*xbar+b));tt8;
qm8=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt8^4*cx^4)-16*lm*tt8^2*cx^2*cy^2*(1-rhoyx^2));qm8;
qn8=64*(1+lm*cy^2*(1-rhoyx^2));qn8;
gk9=qm8/qn8;gk9;
##################################
a=1;a;
b=N*xbar1;b;
tt9=((a*xbar)/(a*xbar+b));tt9;
qm9=lm*ybar^2*(64*cy^2*(1-rhoyx^2)-(lm*tt9^4*cx^4)-16*lm*tt9^2*cx^2*cy^2*(1-rhoyx^2));qm9;
qn9=64*(1+lm*cy^2*(1-rhoyx^2));qn9;
gk10=qm9/qn9;gk10;


########################MSE RESULT####################
vy1;mse2;mse3;mse4;mse5;mse6;singh1;singh2;singh3;singh4;singh5;singh6;singh7;singh8;singh9;singh10;gk1;gk2;gk3;gk4;gk5;gk6;gk7;gk8;gk9;gk10;

#################################################
pre1=(vy1/vy1)*100;pre1;
pre2=(vy1/mse2)*100;pre2;
pre3=(vy1/mse3)*100;pre3;
pre4=(vy1/mse4)*100;pre4;
pre5=(vy1/mse5)*100;pre5;
pre6=(vy1/mse6)*100;pre6;
pre1;pre2;pre3;pre4;pre5;pre6;
#########################
pre7=(vy1/singh1)*100;pre7;
pre8=(vy1/singh2)*100;pre8;
pre9=(vy1/singh3)*100;pre9;
pre10=(vy1/singh4)*100;pre10;
pre11=(vy1/singh5)*100;pre11;
pre12=(vy1/singh6)*100;pre12;
pre13=(vy1/singh7)*100;pre13;
pre14=(vy1/singh8)*100;pre14;
pre15=(vy1/singh9)*100;pre15;
pre16=(vy1/singh10)*100;pre16;

pre7;pre8;pre9;pre10;pre11;pre12;pre13;pre14;pre15;pre16;

######################### Propsed Estr PRE ################
pre17=(vy1/gk1)*100;pre17;
pre18=(vy1/gk2)*100;pre18;
pre19=(vy1/gk3)*100;pre19;
pre20=(vy1/gk4)*100;pre20;
pre21=(vy1/gk5)*100;pre21;
pre22=(vy1/gk6)*100;pre22;
pre23=(vy1/gk7)*100;pre23;
pre24=(vy1/gk8)*100;pre24;
pre25=(vy1/gk9)*100;pre25;
pre26=(vy1/gk10)*100;pre26;
#################################
pre17;pre18;pre19;pre20;pre21;pre22;pre23;pre24;pre25;pre26;

#######################################

pre1;pre2;pre3;pre4;pre5;pre6;pre7;pre8;pre9;pre10;pre11;pre12;pre13;pre14;pre15;pre16;pre17;pre18;pre19;pre20;pre21;pre22;pre23;pre24;pre25;pre26;
