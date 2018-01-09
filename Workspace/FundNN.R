#' Title
#'
#' @return
#' @export
#'
#' @examples
fundNN <- function()
{

set.seed(500)

setwd('~/Documents/2016/RD/NeuralNetwork/Data/Workspace')
#data=read.csv("NWUAX-ver2.csv")

data=read.csv("NWUAX-ver3.csv")


index <- sample(1:nrow(data),round(0.55*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(NAV~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$NAV)^2)/nrow(test)


maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]


library(neuralnet)
n <- names(train_)
f <- as.formula(paste("NAV ~", paste(n[!n %in% "NAV"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),algorithm='rprop+',
                  linear.output=T)
#dev.off()
quartz()
#plot(nn)
plot(nn, rep = NULL, nn.entry = NULL, x.out = NULL,
     radius = 0.20, arrow.length = 0.2, intercept = TRUE,
     intercept.factor = 0.4, information = TRUE,
     information.pos = 0.1, col.entry.synapse = "black",
     col.entry = "black", col.hidden = "red",
     col.hidden.synapse = "black", col.out = "green",
     col.out.synapse = "black", col.intercept = "blue",
     fontsize = 12, dimension = 6, show.weights = FALSE,
     file = NULL)
quartz()

#pr.nn <- compute(nn,test_[,1:8])f
pr.nn <- compute(nn,test_[,1:5])


pr.nn_ <- pr.nn$net.result*(max(data$NAV)-min(data$NAV))+min(data$NAV)
test.r <- (test_$NAV)*(max(data$NAV)-min(data$NAV))+min(data$NAV)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)


print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))
plot(sort(test$NAV))
plot(sort(pr.nn_))
#plot(test$NAV,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='NN',pch=18,col='red', bty='n')
#quartz()
#plot(test$NAV,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
plot(nn)
#quartz()
plot(test$NAV,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$NAV,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

}



firstNN <- function()
{
  x=seq(0,1,by=0.01)
  y=seq(1,2,by=0.01)
  z=0.2+0.4*y^2+0.3*x*sin(15*x)+0.05*sin(50*y)
  inpu=data.frame(q=z,p1=x,p2=y)
  index=sample(1:nrow(inpu),round(.1*nrow(inpu)))
  index=sort(index)
  train=inpu[index,]
  test=inpu[-index,]
  plot(test[,1],type='l',col='red')
 lines(train[,1],type='l',col='blue')
  
 n <- names(train)
 f <- as.formula(paste("q~", paste(n[!n %in% "q"], collapse = " + ")))
 nn <- neuralnet(f,data=train,hidden=c(5,3),algorithm='rprop+',
                 linear.output=T)
 
 pr.nn <- compute(nn,test[,2:3])
 
 
 
 lines(pr.nn$net.result,col='green')
 
}


sFundNN <- function()
{
  library(neuralnet)
  set.seed(1)
  
  setwd('~/Documents/2016/RD/NeuralNetwork/Data/Workspace')
#  data=read.csv("NWUAX-ver2.csv")
  
  data=read.csv("NWUAX-ver3.csv")
  
  #data=read.csv("market-nwuax.csv")
  
  
  index <- sample(1:nrow(data),round(0.6*nrow(data)))
  index=sort(index)
  
  maxs <- apply(data, 2, max) 
  mins <- apply(data, 2, min)
  
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  
  train_ <- scaled[index,]
  test_ <- scaled[-index,]
   
  
  train <- data[index,]
  test <- data[-index,]
 
  plot(test_[,6],type='l',col='red',ann=F)
  #lines(train_[,6],type='l',col='blue')

  box()
  
  # Create a title with a red, bold/italic font
  title(main="Actual Vs Neural Network predictions", col.main="blue", font.main=4)
  
  # Label the x and y axes with dark green text
  
  title(xlab="Time",outer=F, col.lab=rgb(0,0.5,0))
  title(ylab="NAV - Scaled", col.lab=rgb(0,0.5,0))
  
  # Create a legend at (1, g_range[2]) that is slightly smaller 
  # (cex) and uses the same line colors and points used by 
  # the actual plots 
  
  n <- names(train)
  f <- as.formula(paste("NAV ~", paste(n[!n %in% "NAV"], collapse = " + ")))
  nn <- neuralnet(f,data=train_,hidden=c(3,2),linear.output = T)
  pr.nn <- compute(nn,test_[,1:5])
  lines(pr.nn$net.result,type="l",col='blue')
 
  legend( 0,1,c("Actual","NN Pred."), cex=0.8, 
         col=c("red","blue"), pch=21:22, lty=1:2);
  
 # quartz()
  lm.fit <- glm(NAV~., data=train_)
  summary(lm.fit)
  
  pr.lm <- predict(lm.fit,test_)
  
  plot(test_[,6],type='l',col='red',ann=F)
  #lines(train_[,6],type='l',col='blue')
  lines(pr.lm,type='l',col='blue')
  
  box()
  
  # Create a title with a red, bold/italic font
  title(main="Actual Vs GLM predictions", col.main="blue", font.main=4)
  
  # Label the x and y axes with dark green text
  
  title(xlab="Time",outer=F, col.lab=rgb(0,0.5,0))
  title(ylab="NAV Scaled", col.lab=rgb(0,0.5,0))
  
  legend( 0,1,c("Actual","GLM Pred."), cex=0.8, 
          col=c("red","blue"), pch=21:22, lty=1:2);
  
  
  
  pr.nn_ <- pr.nn$net.result*(max(data$NAV)-min(data$NAV))+min(data$NAV)
  test.r <- (test_$NAV)*(max(data$NAV)-min(data$NAV))+min(data$NAV)
  
  #MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
  
  MSE.nn <- sum((pr.nn$net.result - test_$NAV)^2)/nrow(test_)
  
  print(MSE.nn)
  
  MSE.lm <- sum((pr.lm - test_$NAV)^2)/nrow(test_)
  print(MSE.lm)
  
  
  plot(nn, rep = NULL, nn.entry = NULL, x.out = NULL,
       radius = 0.20, arrow.length = 0.2, intercept = TRUE,
       intercept.factor = 0.4, information = TRUE,
       information.pos = 0.1, col.entry.synapse = "black",
       col.entry = "black", col.hidden = "red",
       col.hidden.synapse = "black", col.out = "green",
       col.out.synapse = "black", col.intercept = "blue",
       fontsize = 12, dimension = 6, show.weights = FALSE,
       file = NULL)
  #plot(nn)
}
