#O Modelo de Markovitz para Otimiza��o de Carteiras de Investimento. 

'''#Premissas:
1. Risco = Vari�ncia
2. carteira de Investimentos � um conjunto de investimentos de um indiv�duo
3. Retorno da carteira:  varia��o percentual do valor dos ativos na carteira de
investimento num dado horizonte de tempo
4. Magnitude do retorno: � caracterizado pela esperan�a matem�tica do retorno na carteira de
investimentos, que representa a percep��o/entendimento do investidor para o per�odo futuro.
5. Prefer�ncias: os investidores t�m suas prefer�ncias com rela��o a investimentos fundamentadas na
magnitude do retorno e no risco do retorno, caracterizados por suas percep��es quanto �
esperan�a matem�tica e vari�ncia t�orica de seus retornos.
6. E(R(theta)) = theta x m1 + (1 - theta) x m2
7. E(V(R(theta)) = theta^2 x s1^2 + (1 - theta)^2 x s2^2 + 2 x theta x (1 - theta) x r x s1 x s2'''

'''Exemplo: m1 = E(R1), m2=E(R2), 
s1=desvio padrao de R1, s2=desvio padr�o de R2,
r=�ndice de correla��o entre R1 e R2

seja m1=10, m2=12, s1=1, s2=2 e r=-0.5'''

m1 <- 10
m2 <- 12
s1 <- 1
s2 <- 2
r <- -0.5

theta <- seq(0,1,0.05) ## v�rios valores para theta a propor��o do capital em I1

E<-theta*m1+(1-theta)*m2
V<-theta^2*s1^2+(1-theta)^2*s2^2+2*theta*(1-theta)*r*s1*s2

#Gr�fico da Fronteira de efici�ncia do investimento
plot(V,E,type="l",xlab="V(R) - risco ", ylab="E(R) - retorno ",xlim=c(0,4))
title(main="Fronteira Eficiente (2 investimentos)")


'''O pr�ximo gr�fico mostra, dentro das mesmas premissas anteriores, o efeito de se alterar o valor de no
formato da curva. A situa��o ideal � a que atinge o valor minimo poss�vel (-1), mas isso dependeria da
natureza dos investimentos, pois o valor de n�o est� sob o controle do investidor, somente theta est�.'''
for(r in c(-0.9,-0.5,0,0.5,0.9)){
  V<-theta^2*s1^2+(1-theta)^2*s2^2+2*theta*(1-theta)*r*s1*s2
  lines(V,E,type="l")
  text(V[13]-0.1,10.7,as.character(r),col="red",pos=4)
}
text(-0.3,10.7,expression(rho),col="red",cex=1.4)

#Exemplos num�ricos do Modelo Media-Variancia
#Dados do SP500 de 1995 a 2010
rm(list=ls())
dinvest<-read.csv2("http://ihbs.com.br/html/dinvest.csv")
anos<-1928:2010
dinvest[anos>=1995,1:4] ## observa��es de 1995 at� 2010

#Matriz de Correla��o
m<-apply(dinvest,2,mean) ## encontrando a m�dia das colunas (vari�veis do data frame)
m

V<-cov(dinvest) ## matriz de covari�ncia (vari�ncias na diagonal)
V

cor(V) ## matriz de correla��o

'''Os dados mostram que as a��es (pelo SP500) tiveram um retorno m�dio maior (em torno de 8,1%) e que h�
um bom potencial para ganhos de redu��o de risco pela combina��o desses investimentos, em fun��o de
correla��es negativas entre essas classes de ativos. Por exemplo a correla��o entre os retornos entre g e n foi de -0.89'''

#COMPARA��O DE CARTEIRAS
#Supondo que vc parta de valores de theta ao longo dos investimentos. Seja theta1 (0.25, 0.25, 0.25 0.25)
#e theta2 (0.4,0.2,0.2,0.2), qual a melhor carteira?

theta1=c(0.25,0.25,0.25,0.25)
E<-t(theta1)%*%m ## encontrando E[R(theta1)]
Var<-t(theta1)%*%V%*%theta1 ## V[R(theta1)]
DP<-sqrt(t(theta1)%*%V%*%theta1)
cat("E(R)=",E,"V(R)=",Var,"DP(R)=",DP)

theta2=c(0.4,0.2,0.2,0.2)
E<-t(theta2)%*%m ## encontrando E[R(theta2)]
Var<-t(theta2)%*%V%*%theta2 ## V[R(theta2)]
DP<-sqrt(t(theta2)%*%V%*%theta2) ## desvio padr�o
cat("E(R)=",E,"V(R)=",Var,"DP(R)=",DP)

#Nesse caso, a carteira 2 possui uma esperan�a do retorno maior (4,11%, comparado ao de 3,11% de 1) a custo de um
#risco maior (8,88% contra 6,7%)
