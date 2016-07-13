#read in data

aes_8_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.aesEncrypt_8.txt")
aes_10_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.aesEncrypt_10.txt")
aes_12_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.aesEncrypt_12.txt")
aes_14_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.aesEncrypt_14.txt")

#null encryption
arrayCopy_8_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.arrayCopyEncrypt_8.txt")
arrayCopy_10_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.arrayCopyEncrypt_10.txt")
arrayCopy_12_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.arrayCopyEncrypt_12.txt")
arrayCopy_14_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.arrayCopyEncrypt_14.txt")

blowfish_8_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.blowfishEncrypt_8.txt")
blowfish_10_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.blowfishEncrypt_10.txt")
blowfish_12_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.blowfishEncrypt_12.txt")
blowfish_14_full<-scan("~/Documents/Masters/Project/R/data_ns/encryption/encryption.Encrypt.blowfishEncrypt_14.txt")

#discard first 10000 iterations

aes_8 <- aes_8_full[10001:100000]
aes_10 <- aes_10_full[10001:100000]
aes_12 <- aes_12_full[10001:100000]
aes_14 <- aes_14_full[10001:100000]

arrayCopy_8 <- arrayCopy_8_full[10001:100000]
arrayCopy_10 <- arrayCopy_10_full[10001:100000]
arrayCopy_12 <- arrayCopy_12_full[10001:100000]
arrayCopy_14 <- arrayCopy_14_full[10001:100000]

blowfish_8 <- blowfish_8_full[10001:100000]
blowfish_10 <- blowfish_10_full[10001:100000]
blowfish_12 <- blowfish_12_full[10001:100000]
blowfish_14 <- blowfish_14_full[10001:100000]



aes_data<-list()
aes_data[[1]]<- aes_8
aes_data[[2]]<- aes_10
aes_data[[3]]<- aes_12
aes_data[[4]]<- aes_14

aes_names<-list()
aes_names[[1]]<- "aesEncryption n = 2^8"
aes_names[[2]]<- "aesEncryption n = 2^10"
aes_names[[3]]<- "aesEncryption n = 2^12"
aes_names[[4]]<- "aesEncryption n = 2^14"

bf_data<-list()
bf_data[[1]]<- blowfish_8
bf_data[[2]]<- blowfish_10
bf_data[[3]]<- blowfish_12
bf_data[[4]]<- blowfish_14

bf_names<-list()
bf_names[[1]]<- "blowfishEncryption n = 2^8"
bf_names[[2]]<- "blowfishEncryption n = 2^10"
bf_names[[3]]<- "blowfishEncryption n = 2^12"
bf_names[[4]]<- "blowfishEncryption n = 2^14"

ac_data<-list()
ac_data[[1]]<- arrayCopy_8
ac_data[[2]]<- arrayCopy_10
ac_data[[3]]<- arrayCopy_12
ac_data[[4]]<- arrayCopy_14

ac_names<-list()
ac_names[[1]]<- "arrayCopyEncryption n = 2^8"
ac_names[[2]]<- "arrayCopyEncryption n = 2^10"
ac_names[[3]]<- "arrayCopyEncryption n = 2^12"
ac_names[[4]]<- "arrayCopyEncryption n = 2^14"

#------------------PLOTS--------------------------------------

par(mfrow= c(2,2))

# Plot density of aesEncryption for each N
for (i in 1:4){
  data <- aes_data[[i]]
  d<-density(data)
  plot(d , main = paste("Density of ", aes_names[[i]]), xlab = "execution time (ns)", ylab = "Density", col = "red", xlim=c(0,210000))

}

for (i in 1:4){
  data <- aes_data[[i]]
  x<-seq(0,100,0.001)
  h<-hist(data, main = paste("Histogram of ", aes_names[[i]], " \n with Normal Curve"), prob  = T , xlim = c(0, 210000), breaks = 1000)
  curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col = "green")
}

# Plot density of arrayCopyEncryption (null encryption) for each N
for (i in 1:4){
  data <- ac_data[[i]]
  d<-density(data)
  norm_line<-dnorm(length(data),mean=mean(data),sd=sd(data)) 
  #yfit <- yfit*diff(h$mids[1:2])*length(data)) 
  plot(d , main = paste("Density of ", ac_names[[i]]), xlab = "execution time (ns)", ylab = "Density", col = "red", xlim=c(0,60000))
  lines( norm_line, col="blue", lwd=2)
}

for (i in 1:4){
  data <- ac_data[[i]]
  x<-seq(0,100,0.001)
  h<-hist(data, main = paste("Histogram of ", ac_names[[i]], " \n with Normal Curve"), prob  = T , xlim = c(0, 60000), breaks = 1000)
  curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col = "green")
}

# Plot density of blowfishEncryption for each N
for (i in 1:4){
  data <- bf_data[[i]]
  d<-density(data)
  norm_line<-dnorm(length(data),mean=mean(data),sd=sd(data)) 
  #yfit <- yfit*diff(h$mids[1:2])*length(data)) 
  plot(d , main = paste("Density of ", bf_names[[i]]), xlab = "execution time (ns)", ylab = "Density", col = "red", xlim=c(0,800000))
  lines( norm_line, col="blue", lwd=2)
}

#histogram of blowfish encryption at each n with a normal curve fitted
for (i in 1:4){
  data <- bf_data[[i]]
  x<-seq(0,100,0.001)
  h<-hist(data, main = paste("Histogram of ", bf_names[[i]], " \n with Normal Curve"), prob  = T , xlim = c(0, 800000), breaks = 1000)
  curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col = "green")
}

