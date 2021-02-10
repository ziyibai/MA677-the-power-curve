
# This file is about how to conduct the power curve?

# Based on the example on text book, the sample size n is 100. Taking the type 1 error 
# and type 2 error into consideration, I set the amount of cured people m into 
# 69 and 73 respectively.

# To reduce the likelihood of type 1 error and type 2 error, I use the p of 0.8 and 
# 0.6 respectively. 

library(ggplot2)

n <- 100
m <- c(60,69,73,80)
p <- seq(0.4, 1, 1/n)

# I am aware that I should come up with a loop to conduct this curve, but this
# is a little bit hard for me, so I learned the following code from Shen, hao.

for (i in m){
  if(i == m[1]){
    P <- data.frame(p, Cured=paste('P',i,sep = ''),
                    Power=cumsum(dbinom(i,n,p))
                    )
  }
  else {
    P <- rbind(P, data.frame(p,Cured=paste('P',i,sep = ''),
                             Power=cumsum(dbinom(i,n,p))
                           )
    )
  }
}

#plot the outcome
ggplot()+
  geom_rect(aes(xmin=0.6,xmax=0.8,ymin=0.05,ymax=0.95),alpha=0.5)+
  geom_line(aes(p,Power,color=Cured), P)+
  labs(title = "The Power Curve",fill="No.Cured")+
  scale_fill_discrete(labels=c('69','73'))

# According to the graph, the leftmost curve represents only 60 patients been cured,
# the rightmost curve represents 80 patients been cured.
# Then, we can see that only when the number of cured patient is 69 and 73, the power
# curve goes through the left bottom point and the top right point.
