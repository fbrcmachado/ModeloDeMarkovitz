#O Modelo de Markovitz para Otimização de Carteiras de Investimento. 

'''#Premissas:
1. Risco = Variância
2. carteira de Investimentos é um conjunto de investimentos de um indivíduo
3. Retorno da carteira:  variação percentual do valor dos ativos na carteira de
investimento num dado horizonte de tempo
4. Magnitude do retorno: é caracterizado pela esperança matemática do retorno na carteira de
investimentos, que representa a percepção/entendimento do investidor para o período futuro.
5. Preferências: os investidores têm suas preferências com relação a investimentos fundamentadas na
magnitude do retorno e no risco do retorno, caracterizados por suas percepções quanto à
esperança matemática e variância téorica de seus retornos.
6. E(R(theta)) = theta x m1 + (1 - theta) x m2
7. E(V(R(theta)) = theta^2 x s1^2 + (1 - theta)^2 x s2^2 + 2 x theta x (1 - theta) x r x s1 x s2'''

'''Exemplo: m1 = E(R1), m2=E(R2), 
s1=desvio padrao de R1, s2=desvio padrão de R2,
r=índice de correlação entre R1 e R2

seja m1=10, m2=12, s1=1, s2=2 e r=-0.5'''

m1 <- 10
m2 <- 12
s1 <- 1
s2 <- 2
r <- -0.5

theta <- seq(0,1,0.05) ## vários valores para theta a proporção do capital em I1

E<-theta*m1+(1-theta)*m2
V<-theta^2*s1^2+(1-theta)^2*s2^2+2*theta*(1-theta)*r*s1*s2

#Gráfico da Fronteira de eficiência do investimento
plot(V,E,type="l",xlab="V(R) - risco ", ylab="E(R) - retorno ",xlim=c(0,4))
title(main="Fronteira Eficiente (2 investimentos)")


'''O próximo gráfico mostra, dentro das mesmas premissas anteriores, o efeito de se alterar o valor de no
formato da curva. A situação ideal é a que atinge o valor minimo possível (-1), mas isso dependeria da
natureza dos investimentos, pois o valor de não está sob o controle do investidor, somente theta está.'''
for(r in c(-0.9,-0.5,0,0.5,0.9)){
  V<-theta^2*s1^2+(1-theta)^2*s2^2+2*theta*(1-theta)*r*s1*s2
  lines(V,E,type="l")
  text(V[13]-0.1,10.7,as.character(r),col="red",pos=4)
}
text(-0.3,10.7,expression(rho),col="red",cex=1.4)

#Exemplos numéricos do Modelo Media-Variancia
#Dados do SP500 de 1995 a 2010
rm(list=ls())
dinvest<-read.csv2("http://ihbs.com.br/html/dinvest.csv")
anos<-1928:2010
dinvest[anos>=1995,1:4] ## observações de 1995 até 2010

#Matriz de Correlação
m<-apply(dinvest,2,mean) ## encontrando a média das colunas (variáveis do data frame)
m

V<-cov(dinvest) ## matriz de covariância (variâncias na diagonal)
V

cor(V) ## matriz de correlação

'''Os dados mostram que as ações (pelo SP500) tiveram um retorno médio maior (em torno de 8,1%) e que há
um bom potencial para ganhos de redução de risco pela combinação desses investimentos, em função de
correlações negativas entre essas classes de ativos. Por exemplo a correlação entre os retornos entre g e n foi de -0.89'''

#COMPARAÇÃO DE CARTEIRAS
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
DP<-sqrt(t(theta2)%*%V%*%theta2) ## desvio padrão
cat("E(R)=",E,"V(R)=",Var,"DP(R)=",DP)

#Nesse caso, a carteira 2 possui uma esperança do retorno maior (4,11%, comparado ao de 3,11% de 1) a custo de um
#risco maior (8,88% contra 6,7%)
