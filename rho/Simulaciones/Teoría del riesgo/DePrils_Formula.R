list.of.packages <- c("ggplot2", "reshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

########### LIBS ###############
require(reshape)
require(ggplot2)
################################

############################### SIMULATION ########################################################################
# Simulating probabilities of n identically distributed random variables taking three values corresponding
# to possible sinisters

#### Pars ############################

n<-3 #Number of random variables X1,X2,...Xn
f0<-1/n #Probability of P(X=0)

#######################################

f<-list(NULL)
names<-list(NULL)

probs<-data.frame(Values=c(0,1,2))


for(i in 1:n){
  if(i==1){
r<-runif(2,0,1)
a<-f0
b<-r[2]
rand1 <- min(a, b)
rand2 <- abs(a - b)
rand3 <- 1 - max(a, b)
  f[[i]]<-c(rand1,rand2,rand3)
  probs<-cbind(probs,f[[i]])
  names[[i+1]]<-paste("X",i,sep = "")
  }else{
    f[[i]]<-c(rand1,rand2,rand3)
    probs<-cbind(probs,f[[i]])
    names[[i+1]]<-paste("X",i,sep = "")
  }
}
probs<-`names<-`(probs,names)  
mprobs<-melt(probs,id.vars = 1)
names(mprobs)<-c("values","variable","value")

ggplot(mprobs,aes(x = values,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+labs(x="Valor",y="P(X=Valor)",
                                           title = "Distribución individual")+theme_bw()

#################################### MANUAL ASSIGNMENT #########################################################
# Manual assignement of probabilities <p>, corresponding to <n> random variables taking <Values> as the number
# of possible sinisters, also ## Check provides a way to check if everything's fine.

#### Values and probs ####

Values<-0:5                                    #Number of possible sinisters
n<-200                                          #Number of random variables X1,X2,...,Xn
p<-rep(1/length(Values),length(Values))         #P(X=j) for j in Values
probs<-data.frame(Values,p)

###Check
if(isTRUE(length(Values)!=length(p)) | isTRUE(sum(p)!=1)){
  print("Something's wrong!")
  if(isTRUE(length(Values)!=length(p))){
    print("Distinc number of posible sinisters and probabilities, correspondence must by one to one")
  } else{
    print("Densities always sum to one")
  }
}

mprobs<-melt(probs,id.vars = 1)
ggplot(mprobs,aes(x = Values,y = value)) + 
  geom_bar(stat = "identity",position = "dodge")+labs(x="Valor",y="P(X=Valor)",
                          title = "Distribución individual")+theme_bw()

#############################################################################################################
############### De Pril recursive method ####################################################################
#############################################################################################################

# Recursive De Pril's method to find the risk of n identically distributed random variables (convolution)

g<-list()

for (i in 0:(max(probs$Values)*n)) {
  s<-0
  if(i==0){
    g$cero<-(f0)^n
  }else{
    j<-1
    while(j<=i){
      if(isTRUE(i-j<0) | isTRUE(j>2)){j<-j+1 
      next}
      if (i==j){
        s[j]<-(1/f0)*(((j*(n+1)/i)-1)*probs[j+1,2]*g$cero)
      }else{
      s[j]<-(1/f0)*(((j*(n+1)/i)-1)*probs[j+1,2]*g[['probs']][i-j])
      }
    j<-j+1
    }
    g[['probs']][i]<-sum(s)
  }
}

gprobs<-as.data.frame(cbind(Values=0:(max(probs$Values)*n),probs=c(g$cero,g$probs)))
mprobs<-melt(gprobs,id.vars = 1)
ggplot(mprobs,aes(x = Values,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+labs(x="Valor",y="P(S=Valor)",
                title = "Distribución de la convolución")+theme_bw()

