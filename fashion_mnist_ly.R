library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# CSV FILES DOWNLOADED FROM 
# https://www.kaggle.com/zalando-research/fashionmnist

file_path <- 'C:/downloads/docs/data622/final/'

train_fn <- 'fashion-mnist_train.csv'
test_fn <- 'fashion-mnist_test.csv'

train_path <- str_c(file_path, train_fn)
test_path <- str_c(file_path, test_fn)

train <- read.csv(train_path)
test <- read.csv(test_path)

train$label <- factor(train$label, labels = c('T-shirt/top',
                                              'Trouser',
                                              'Pullover',
                                              'Dress',
                                              'Coat', 
                                              'Sandal',
                                              'Shirt',
                                              'Sneaker',
                                              'Bag',
                                              'Ankle boot'))

test$label <- factor(test$label, labels = c('T-shirt/top',
                                            'Trouser',
                                            'Pullover',
                                            'Dress',
                                            'Coat', 
                                            'Sandal',
                                            'Shirt',
                                            'Sneaker',
                                            'Bag',
                                            'Ankle boot'))

plot_row <- function(row, data = train) {
  
  # dataframe in parameter
  df <- data[row,]
  
  # store label
  label <- df$label
  
  # remove label
  df <- df %>%
    select(-label)
  
  # change field names to seq 1 to 784
  colnames(df) <- seq_len(28*28)
  
  # add back label
  df$label <- label
  
  # gather fields into rows
  df2 <- gather(df, var, val, -label)
  
  # convert col name variable into integer
  df2$var <- as.integer(df2$var)
  
  # calculate grid
  df2$y <- ceiling(df2$var / 28)
  df2$x <- df2$var - 28 * (df2$y - 1)
  
  # plot
  p <- ggplot(df2, aes(x = x, y = y, fill = val)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          aspect.ratio = 1,
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'none') +
    scale_y_reverse() +
    labs(x = element_blank(),
         y = element_blank()) +
    facet_wrap(~label)
  
  return(p)
}

plot_row(2)
plot_row(2, test)



library(caret)

ctrl <- trainControl(method = 'cv', number = 10)
grid <-  expand.grid(mtry = c(3,4,7), splitrule = "variance", min.node.size = 5)

c <- train(label ~ .,
           data = train,
           method = 'svmRadial',
           trControl = ctrl
           )





