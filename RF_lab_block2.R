#install.packages("randomForest")
library(randomForest)
library(dplyr)





print(miss_df %>% round(4))



random_forest_sim <- function(NS = 25, condition = function(x1, x2){
  x1 < x2
  
}) {
  
  # ------------------------
  
  set.seed(1234)
  x1<-runif(1000)
  x2<-runif(1000)
  tedata<-cbind(x1,x2)
  y<-as.numeric(condition(x1, x2))
  telabels<-as.factor(y)
  
  
  
  # ------------------------
  
  
  ntree <- c(1, 10, 100)
  n <- 1000
  
  miss_df <- matrix(0, 3, length(ntree)) %>%  as.data.frame()
  rownames(miss_df) <- c("mu", "var", "sd")
  colnames(miss_df) <- ntree
  
  
  for (nt in ntree) {
    missrate <- rep(1, n)
    for (i in 1:n) {
      
      x1<-runif(100)
      x2<-runif(100)
      trdata<-cbind(x1,x2)
      y<-as.numeric(condition(x1, x2))
      trlabels<-as.factor(y)
      
      model <- randomForest(x = trdata, y = trlabels, ntree = nt, keep.forest = TRUE, 
                            nodesize = NS)
      
      y_hat <- predict(model, tedata)
      missrate[i] <-  mean(y_hat != telabels)
      
    }
    
    miss_df[1, which(nt == ntree)] <- mean(missrate)
    miss_df[2, which(nt == ntree)] <- var(missrate)
    miss_df[3, which(nt == ntree)] <- sd(missrate)
  }
  
  return(miss_df)
}

# x1 < x2
rf1 <- random_forest_sim()

rf1 %>% round(4)


# x1 < 0.5
rf2 <- random_forest_sim(condition = function(x1, x2){
  x1 < 0.5
})

rf2 %>% round(4)

# Much easier task, higher variance for 1 tree, interesting

# divide the domain into 4 equal squares, top right and bottom left is 1, otherwise 0.
rf3 <- random_forest_sim(NS = 25, condition = function(x1, x2){
  (x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)
})

rf3 %>% round(4)



rf1 %>% round(4)
rf2 %>% round(4)
rf3 %>% round(4)


What happens with the mean error rate when the number of trees in the random
forest grows? Why?

# Q1
# it drops
  
  – The third dataset represents a slightly more complicated classification problem
than the first one. Still, you should get better performance for it when using suffi-
  cient trees in the random forest. Explain why you get better performance.


My guess would be that the class boundries are parallell to the variable axises. 
random forests approximates curves by many discrete steps. 

