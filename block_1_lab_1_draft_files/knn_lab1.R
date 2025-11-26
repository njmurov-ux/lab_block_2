data <- read.csv("C:/Users/arone/Desktop/MasterML/optdigits.csv")

library(dplyr)
library(ggplot2)
library(kknn)
library(reshape2)
library(tidyverse)
library(ggpubr)

# last column 
sapply(1:ncol(data), function(x) data[, x] %>% unique() %>% length())


colnames(data)[65] <- "y"
data$y <- data$y %>% as.factor()


n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]

# the ids not in id
id1=setdiff(1:n, id)

set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,]
# the rest is test

# ------------------------------

model <- kknn(y ~ ., train = train, test = valid, k = 30, kernel = "rectangular")
y_hat_v <- model$fitted.values 

model <- kknn(y ~ ., train = train, test = train, k = 30, kernel = "rectangular")
y_hat_t <- model$fitted.values 


conf_table <- function(y, y_hat, decimals = 2, p_or_r) {
  
  cm <- table(Number = y, Predict = y_hat) 
  
  C <- nrow(cm)
  # I wanna do relative coloring, based on deviation
  rowsums <- sapply(1:C, function(x) sum(cm[x, ]))
  perfect_pred <- diag(rowsums)
  
  diff <- cm - perfect_pred
  
  # If one row has more examples than another we need to scale to see devation
  for (i in 1:nrow(diff)) {
    diff[i, ] <- diff[i, ] / rowsums[i]
    
  }
  
  # I dont care about the diagonal, the most interesting is that the errors pop out
  diff[diff < 0] <- 0

  
  cm_long <- melt(cm)
  cm_long$Number <- cm_long$Number[nrow(cm_long):1] %>% as.factor()
  cm_long$Predict <- cm_long$Predict %>% as.factor()
  
  cm_long$color <- melt(diff)$value
  
  
  
  ggplot(cm_long, aes(x = Predict, y = Number, fill = color)) +
    geom_tile(color = "black") +
    geom_text(aes(label = value), color = "black", size = 15) +
    scale_fill_gradient(low = "white", high = "#3182bd", guide = "none") +
    theme_minimal(base_size = 45) +
    scale_y_discrete(labels = levels(cm_long$Number) %>% rev() ) +
    labs(x = "Predicted", y = "Actual", title = paste0("Missclassification rate = ", 
                                                    (1 - mean(y == y_hat)) %>% 
                                                      round(decimals)) ) +
    theme(
      axis.text.x = element_text(hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid = element_blank(),
    )
}

conf_table(train$y, y_hat_t)
conf_table(valid$y, y_hat_v)


missclass_table <- function(y, y_hat) {
  
  cm <- table(Number = y, Predict = y_hat)
  FN <- sapply(1:nrow(cm), function(x) (sum(cm[x, ]) - cm[x, x]))
  TP <- diag(cm)
  FP <- sapply(1:ncol(cm), function(x) (sum(cm[, x]) - cm[x, x]))
  
  # when predicting class X how many times was right?
  Prescision <- TP / (TP + FP)
  
  # when the class was X how often was X predicted?
  Recall <- TP / (TP + FN)
  
  miss_table <- rbind(Prescision, Recall) %>% round(2)
  
  print("Prescision: when predicting class X how many times was right?")
  print("Recall: when the class was X how often was X predicted?")
  return(miss_table)
  
}


missclass_table(train$y, y_hat_t)
missclass_table(valid$y, y_hat_v)


1 - (train$y == y_hat_t) %>% mean() %>% round(2)
1 - (valid$y == y_hat_v) %>% mean() %>% round(2)



# ----------------------------------------------
# Ex 3
prob_eight <- model$prob[, 9][y_hat_t == 8]
ordering <- prob_eight[order(prob_eight)]
hard <- ordering[1:3]
easy <- ordering[(length(ordering)-1):length(ordering)]

hard_index <- which(prob_eight %in% hard)
easy_index <- which(prob_eight %in% easy)[1:2]


viz_number <- function(index, title, df = train, which_num = 8) {
  df <- train[y_hat_t == which_num, ]
  vec <- df[index, 1:64]
  viz_matrix <- matrix(0, 8, 8)
  for (i in 1:8) {
    viz_matrix[i, ] <- vec[(8*(i-1) + 1):(8*i)] %>% as.matrix()
  }
  
  M_long <- melt(viz_matrix)
  
  ggplot(M_long, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(value, 2)), color = "white", size = 15) +
    scale_fill_gradient(low = "white", high = "black", guide = "none") +
    theme_minimal(base_size = 45) +
    scale_y_discrete(limits = rev(levels(M_long$Var1))) +
    labs(x = "", y = "", title = title) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.title  = element_text(hjust = 0.5, face = "bold"),
      panel.grid  = element_blank()
    )
  
  
}

ggarrange(
viz_number(easy_index[1], "Easy 8s"),
viz_number(easy_index[2], "")
)

ggarrange(
  viz_number(hard_index[1], ""),
  viz_number(hard_index[2], "Hard 8s"),
  viz_number(hard_index[3], ""),
  ncol = 3
)


train_error <- c()
validation_error <- c()

for (k in 1:30) {
  if (k%%5 == 0) {
    print(k)
  }
  model <- kknn(y ~ ., train = train, test = valid, k = k, kernel = "rectangular")
  y_hat_v <- model$fitted.values 
  
  model <- kknn(y ~ ., train = train, test = train, k = k, kernel = "rectangular")
  y_hat_t <- model$fitted.values 
  
  train_error <- c(train_error, 1 - (train$y == y_hat_t) %>% mean())
  validation_error <- c(validation_error, 1 - (valid$y == y_hat_v) %>% mean())
  
}

plot_df <- data.frame(Validation = validation_error, Train = train_error, 
                      k = 1:length(train_error))
ggplot(plot_df) + aes(x = k) + geom_line(aes(y = Validation, color = "Validation")) + 
  geom_line(aes(y = Train, color = "Train")) + theme_bw(base_size = 45) + 
  scale_color_manual(values = c("Validation" = "red", "Train" = "black")) +
  geom_vline(xintercept = which.min(validation_error), linetype = "dashed", color = "grey", linewidth = 1.5) + 
  labs(y = "Missclassification error", color = "Dataset") + theme(
    axis.text.x = element_text(hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(size = 45),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),
  ) + annotate("text", x = 17, y = 0.01, 
               label = paste0("Optimal k = ", which.min(validation_error)), size = 15)


model <- kknn(y ~ ., train = train, test = test, 
              k = which.min(validation_error), kernel = "rectangular")
y_hat_test <- model$fitted.values 

(y_hat_test == test$y) %>% mean() 

missclass_table(test$y, y_hat_test)
conf_table(test$y, y_hat_test)
# 7 and 9 predicted too often, 4s are the hardest to detect, 
# maybe theyre written like mu and 4


cross_error <- c()
for (k in 1:30) {
  if (k%%5 == 0) {
    print(k)
  }
  model <- kknn(y ~ ., train = train, test = valid, k = k, kernel = "rectangular")
  y_hat_ <- model$fitted.values
  
  
  # x + 1 as the indexing starts at 1
  loss_per_class <- sapply(0:9, function(x) {
    neg_loss <- (model$prob[y_hat_v == x, x + 1]  + 10e-15) %>% 
           log() %>% sum()
    return(-neg_loss)
    } 
    )
  
  
  cross_error <- c(cross_error, sum(loss_per_class))
  
}

plot(cross_error)
which.min(cross_error)


plot_df <- data.frame(Validation = cross_error, 
                      k = 1:length(cross_error))
ggplot(plot_df) + aes(x = k) + geom_line(aes(y = Validation), color = "black") + 
  theme_bw(base_size = 45) + 
  geom_vline(xintercept = which.min(cross_error), linetype = "dashed", color = "grey", linewidth = 1.5) + 
  labs(y = "Cross-entropy loss") + theme(
    axis.text.x = element_text(hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(size = 45),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),
  ) + annotate("text", x = 17, y = 1000, 
               label = paste0("Optimal k = ", which.min(cross_error)), size = 15)
