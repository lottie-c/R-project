#install.packages("gridExtra")
#install.packages("grid")
#install.packages("gtable")

library(gridExtra)
library(grid)
library(gtable)
#sample size will be one of 500, 1000, 2000, 10000, 50000
ks_test <- function( x, y, sampleSize, lambdaX = 1, lambdaY = 1){
  
  if(length(y) < length(x)){
    data_length <- length(y)
  }else{
    data_length <- length(x)
  }
  if (sampleSize > data_length){
    stop("sampleSize must be less than or equal to the length of the data ")
  }
  
  #x<-x[1:sampleSize]
  # y<-y[1:sampleSize]
  
  x<-x*lambdaX
  y<-y*lambdaY
  
  # "greater" tests with alternative that x lies to left of y 
  p_value <- ks.test(x,y, alternative = "greater")$p.value
  return (p_value)
}

t_test <- function( x, y, sampleSize, lambdaX = 1, lambdaY = 1){
  
  if(length(y) < length(x)){
    data_length <- length(y)
  }else{
    data_length <- length(x)
  }
  if (sampleSize > data_length){
    stop("sampleSize must be less than or equal to the length of the data ")
  }
  
  x<-x[1:sampleSize]
  y<-y[1:sampleSize]
  
  x<-x*lambdaX
  y<-y*lambdaY
  
  # "less" tests with alternative that x has a lower mean than y 
  p_value <- t.test( x, y, alternative = "less")$p.value
  return (p_value)
}

mww_test <- function( x, y, sampleSize, lambdaX = 1, lambdaY = 1){
  
  if(length(y) < length(x)){
    data_length <- length(y)
  }else{
    data_length <- length(x)
  }
  if (sampleSize > data_length){
    stop("sampleSize must be less than or equal to the length of the data ")
  }
  
  x<-x[1:sampleSize]
  y<-y[1:sampleSize]
  
  
  x<-x*lambdaX
  y<-y*lambdaY
  
  # "less" tests with alternative that the mean of x is shifted to left of
  #the mean of y 
  p_value <- wilcox.test(x,y, paired = FALSE, alternative = "less")$p.value
  return (p_value)
}


# proportion() outputs the proportion of the time that the null hypothesis,  
# that thedistributions have equal means, is rejected by 'test' with sample 
# distributions x and y. 
#x = sample distribution, y = sample distribution 
# test = c("ks","mww","t"), for Kolmogorov Smirnov, wilcox and welch's t test
# sampleSize = sample size for each test, lambdaX and lambdaY are multipliers
#used in SPL

proportion <- function( x, y, test = c("ks", "t", "mww"), sampleSize = 500, 
                        lambdaX = 1, lambdaY = 1){
  
  possible_tests <- c("ks", "t", "mww")
  if(!(test %in% possible_tests)){
    stop("Incorrect test input, test should be 'ks', 't' or 'mww'")
  }
  
  if(length(y) < length(x)){
    data_length <- length(y)
  }else{
    data_length <- length(x)
  }
  
  if (sampleSize > data_length){
    stop("sampleSize must be less than or equal to the length of the data ")
  }
  
  start <- 1
  end <- sampleSize
  j <- 1
  times <- (data_length/sampleSize)
  values<-rep(0,times)  
  
  #times <- 10
  for (i in 1:times){
    dist1 <- x[start:end]
    dist2 <- y[start:end]
    
    if( test == "ks"){
      p_value <- ks_test(dist1, dist2, sampleSize, lambdaX, lambdaY)
    }else if( test == "mww"){
      p_value <- mww_test(dist1, dist2, sampleSize, lambdaX, lambdaY)
    }else {
      p_value <- t_test(dist1, dist2, sampleSize, lambdaX, lambdaY)
    }
    
    if(p_value <= 0.05){
      values[j] <- 0
    }else{
      values[j] <- 1
    }
    
    j<- j + 1
    start <- start + sampleSize
    end <- end + sampleSize
  }
  power<-sum(values)/length(values)
  return(power)
  
}

#function outputing p-values for a one sided, kolmogorov-smirnov test,
# mann whitneyu test and welch's t-test where the alternative hypothesis 
# is that  data1 has smaller values than data2.
# data1, like data2, is a list of vectors containing data
# sampleSizes is a vector of the sample sizes to test
#lambdaX and lambdaY are multipliers for the data

p_value_table<- function( data1, data2, sampleSizes, lambdaX=1, lambdaY=1){
  
  if(length(data2) < length(data1)){
    data_length <- length(data2)
  }else{
    data_length <- length(data1)
  }

  sampleSizes_length <- length(sampleSizes)
  
  ks_vec<-rep(0,sampleSizes_length)
  mww_vec<-rep(0,sampleSizes_length)
  t_vec<-rep(0,sampleSizes_length)
  
  for (i in 1:sampleSizes_length){
    
    
    x<-data1
    y<-data2
    
    sampleSize <- sampleSizes[i] 
    
    ks <- ks_test( x, y, sampleSize, lambdaX, lambdaY)
    if (ks != 0 ){
      ks <- format(ks, digits = 3, scientific = T)
    }  
    ks_vec[i]<-ks
    
    mww <- mww_test( x, y, sampleSize, lambdaX, lambdaY)
    if (mww != 0 ){
      mww <- format(mww, digits = 3, scientific = T)
    }  
    mww_vec[i]<- mww
    
    t <- t_test( x, y, sampleSize, lambdaX, lambdaY)
    if (t != 0 ){
      t <- format(t, digits = 3, scientific = T)
    }  
    t_vec[i]<-t 
    
  }
  
  output <- data.frame( sampleSizes, ks_vec, mww_vec, t_vec)
  
}



p_tables_different_n <- function( data_vec1, data_vec2, names_vec1, names_vec2,
                                  sampleSizes, lambdaX=1, lambdaY=1){
  
  if (length(data_vec2) < length(data_vec1)){
    length_data_vec <- length(data_vec2)
  }else{
    length_data_vec <- length(data_vec1)
  }
  
  tables_list <- list()
  
  for (i in 1: length_data_vec){
    table <- p_value_table( data_vec1[[i]], data_vec2[[i]], sampleSizes, 
                            lambdaX=1, lambdaY=1)
    
  
    if (lambdaX == 1){
      if(lambdaY == 1){
        title <- textGrob(paste( "p-value for", names_vec1[[i]],
                                 "<", names_vec2[[i]]) 
                          ,gp=gpar(fontsize=10))
      }else{
        title <- textGrob(paste( "p-value for", names_vec1[[i]],
                                 "<", lambdaY, "*", names_vec2[[i]]) 
                          ,gp=gpar(fontsize=10))
      }
    }else if (lambdaY == 1){
      title <- textGrob(paste( "p-value for", lambdaX, "*", names_vec1[[i]],
                             "<", names_vec2[[i]]) 
                     ,gp=gpar(fontsize=10))
    }else{
      title <- textGrob(paste( "p-value for", lambdaX, "*", names_vec1[[i]],
                             "<", lambdaY, "*", names_vec2[[i]]) 
                      ,gp=gpar(fontsize=10))
    }
    padding <- unit(3,"mm")
    table <- tableGrob(table, rows = NULL)
    table <- gtable_add_rows(table, 
                             heights = grobHeight(title) + padding,
                             pos = 0)
    table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table))
    
    tables_list[[i]] <- table
  }
  grid.arrange(
     tables_list[[1]],
     tables_list[[2]],
     tables_list[[3]],
     tables_list[[4]],
    nrow = length_data_vec/2
    )
   
}

#Function to draw histogram with normal distribution over layed
# input data - list of vectors, each vector contains data for one plot
# input names - list of strings, each string gives the title 
hist_norm <- function(input_data, input_names){
  length_input <- length(input_data)
  for (i in 1:length_input){
    data <- input_data[[i]]
    x<-seq(0,100,0.001)
    h<-hist(data, main = paste("Histogram of ", input_names[[i]], 
                               " \n with Normal Curve"), prob  = T , 
            xlim = c(min(data)  , max(data)/2.5), breaks = 1000)
    curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col = "green")
  }
}

#Function to draw density plots of input data
# input data - list of vectors, each vector contains data for one plot
# input names - list of strings, each string gives the title 
density_plot <- function(input_data, input_names){
  length_input<- length(input_data)
  par(mfrow =c(length/length_input/2,2))
  for (i in 1:length_input ){
    data <- input_data[[i]]
    d<-density(data)
    plot(d , main = paste("Density of ", input_names[[i]]),
         xlab = "execution time (ns)", ylab = "Density", 
         col = "red", xlim=c(0,max(data) + max(data)/100))
  }
} 

