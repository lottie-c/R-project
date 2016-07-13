source('~/Documents/Masters/Project/R/test_functions.R')

# read in data 
dualPivotSort_100<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.dualPivotSort_100.txt")
dualPivotSort_500<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.dualPivotSort_500.txt")
dualPivotSort_1000<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.dualPivotSort_1000.txt")
dualPivotSort_5000<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.dualPivotSort_5000.txt")
 
insertionSort_100<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.insertionSort_100.txt")
insertionSort_500<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.insertionSort_500.txt")
insertionSort_1000<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.insertionSort_1000.txt")
insertionSort_5000<- scan("~/Documents/Masters/Project/R/data_ns/sort/sorting.Sort.insertionSort_5000.txt")

# discard first 10000 results
dualPivotSort_100 <- dualPivotSort_100[10001:110000]
dualPivotSort_500 <- dualPivotSort_500[10001:110000]
dualPivotSort_1000 <- dualPivotSort_1000[10001:110000]
dualPivotSort_5000 <- dualPivotSort_5000[10001:110000]

insertionSort_100<- insertionSort_100[10001:110000]
insertionSort_500<- insertionSort_500[10001:110000]
insertionSort_1000<- insertionSort_1000[10001:110000]
insertionSort_5000<- insertionSort_5000[10001:110000]



is_data <- list()
is_data[[1]] <- insertionSort_100
is_data[[2]] <- insertionSort_500
is_data[[3]] <- insertionSort_1000
is_data[[4]] <- insertionSort_5000

ps_data <- list()
ps_data[[1]] <- dualPivotSort_100
ps_data[[2]] <- dualPivotSort_500
ps_data[[3]] <- dualPivotSort_1000
ps_data[[4]] <- dualPivotSort_5000

ps_names<- list()
ps_names[[1]] <- "dualPivotSort n=100"
ps_names[[2]] <- "dualPivotSort n=500"
ps_names[[3]] <- "dualPivotSort n=1000"
ps_names[[4]] <- "dualPivotSort n=5000"

is_names <- list()
is_names[[1]] <- "insertionSort n=100"
is_names[[2]] <- "insertionSort n=500"
is_names[[3]] <- "insertionSort n=1000"
is_names[[4]] <- "insertionSort n=5000"


hist(insertionSort_5000)

edf_ps_100<-ecdf(dualPivotSort_100)
edf_ps_500<-ecdf(dualPivotSort_500)
edf_ps_1000<-ecdf(dualPivotSort_1000)
edf_ps_5000<-ecdf(dualPivotSort_5000)

edf_is_100<-ecdf(insertionSort_100)
edf_is_500<-ecdf(insertionSort_500)
edf_is_1000<-ecdf(insertionSort_1000)
edf_is_5000<-ecdf(insertionSort_5000)


par(mfrow=c(2,2))
#title(main = "Comparing the Emperical distribution formulae of dualPivotSort and insertionSort",  side = 3, line = -21)
plot(edf_ps_100, main = "n = 100",
     col.01line = "gray70", pch = 19, col = "red" , verticals = "TRUE")
  lines(edf_is_100,  
     col.01line = "gray70", pch = 19, col = "blue", verticals = "TRUE")

plot(edf_ps_500, main = "n = 500",
       col.01line = "gray70", pch = 19, col = "red", verticals = "TRUE")
  lines(edf_is_500,  
        col.01line = "gray70", pch = 19, col = "blue", verticals = "TRUE")

plot(edf_ps_1000,main = "n = 1000",
       col.01line = "gray70", pch = 19, col = "red", verticals = "TRUE")
lines(edf_is_1000,  
        col.01line = "gray70", pch = 19, col = "blue", verticals = "TRUE")

plot(edf_ps_5000,main = "n = 5000",
     col.01line = "gray70", pch = 19, col = "red", xlim=c(-1, 15000000), verticals = "TRUE")
lines(edf_is_5000,  
      col.01line = "gray70", pch = 19, col = "blue", verticals = "TRUE")

density_plot <- function(input_data, input_names){
  length_input<- length(input_data)
  for (i in 1:length_input ){
    data <- input_data[[i]]
    d<-density(data)
    plot(d , main = paste("Density of ", input_names[[i]]),
         xlab = "execution time (ns)", ylab = "Density", col = "red", 
         xlim=c(0,max(data) + max(data)/100))
  }
} 

sampleSizes <- c(100,500,1000,5000,10000)
table <- p_value_table(dualPivotSort_100,  insertionSort_100, 500,1 ,1)
table

a <- p_tables_different_n( ps_data, is_data, ps_names, is_names, sampleSizes)

a



