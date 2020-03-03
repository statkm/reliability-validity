library(epiR)
library(epitools)
library(dplyr)
library(ggplot2)
library(magrittr)
library(knitr)
library(xtable)
library(flextable)
library(officer)
library(readr)

# functions

plot_kappa <- function(tb_dat_ch){
  n_len <- dim(tb_dat_ch)[1]
  plot(0.5,xlim = c(0.5,n_len+.5), ylim = c(0.5,n_len+.5), type="n", xlab = "x", ylab="y", main="Exchanged Contingency Table")
  abline(h=0:n_len+.5); abline(v=0:n_len+.5)
  for(i in 1:n_len){
    for(j in 1:n_len){
      rect(i-.5, j-0.5, i+.5, j+.5, col = rgb(1, (tb_dat_ch[i,j]/sum(tb_dat_ch[i,])),(tb_dat_ch[i,j]/sum(tb_dat_ch[i,])), alpha=0.7) )
      text(i,j, paste(round(tb_dat_ch[i,j]/sum(tb_dat_ch)*100), "%") )
    }
  }
}


kappa_calc <- function(p_vec, FL=1){
  n_len <- length(p_vec)
  p_exp <- sum(p_vec^2)
  
  n_s <- 1000
  if(FL){
    xx <- 1:n_len %*% rmultinom(n=n_s, size=1, prob=p_vec) %>% as.numeric
    yy <- 1:n_len %*% rmultinom(n=n_s, size=1, prob=p_vec) %>% as.numeric
  }else{
    xx <- 1:n_len %*% rmultinom(n=n_s, size=1, prob=p_vec) %>% as.numeric
    yy <- xx
  }
    
  
  df_dat <-data.frame(xx=xx, yy=yy)
  
  tb_dat <- matrix(0, ncol=n_len, nrow = n_len)
  
  for(i in 1:n_len)
    for(j in 1:n_len)
      tb_dat[i,j] <- dim( df_dat[which(xx==i & yy==j),] )[1]
  
  tb_dat_ch <- tb_dat+t(tb_dat)
  
  p_obs <- sum(diag(tb_dat_ch))/sum(tb_dat_ch)
  
  
  kappa <- (p_obs-p_exp)/(1-p_exp)
  return(list(p_vec=p_vec, p_exp=p_exp, p_obs=p_obs, tb_dat=tb_dat, tb_dat_ch=tb_dat_ch, kappa=kappa))
}

kappa_calc_2 <- function(p_vec1, p_vec2){
  n_len <- length(p_vec1)
  p_exp <- sum(p_vec1^2)
  
  n_s <- 1000
  xx <- 1:n_len %*% rmultinom(n=n_s, size=1, prob=p_vec1) %>% as.numeric
  yy <- 1:n_len %*% rmultinom(n=n_s, size=1, prob=p_vec2) %>% as.numeric

  
  df_dat <-data.frame(xx=xx, yy=yy)
  
  tb_dat <- matrix(0, ncol=n_len, nrow = n_len)
  
  for(i in 1:n_len)
    for(j in 1:n_len)
      tb_dat[i,j] <- dim( df_dat[which(xx==i & yy==j),] )[1]
  
  tb_dat_ch <- tb_dat+t(tb_dat)
  
  p_obs <- sum(diag(tb_dat_ch))/sum(tb_dat_ch)
  
  
  kappa <- (p_obs-p_exp)/(1-p_exp)
  return(list(p_vec1=p_vec1, p_vec2=p_vec2, p_exp=p_exp, p_obs=p_obs, tb_dat=tb_dat, tb_dat_ch=tb_dat_ch, kappa=kappa))
}



## under the same model

p_vec <- c(0.3, 0.3, 0.4)

result_kappa <- kappa_calc(p_vec, FL=TRUE)
result_kappa
plot_kappa(result_kappa$tb_dat_ch)


## exact

p_vec <- c(0.3, 0.3, 0.4)
result_kappa
result_kappa<- kappa_calc(p_vec, FL=0)
plot_kappa(result_kappa$tb_dat_ch)


## different

p_vec1 <- c(0.3, 0.3, 0.4)
p_vec2 <- c(0.3, 0.2, 0.5)
result_kappa <- kappa_calc_2(p_vec1, p_vec2)
result_kappa
plot_kappa(result_kappa$tb_dat_ch)


p_vec1 <- c(0.3, 0.3, 0.4)
p_vec2 <- c(0.3, 0.2, 0.5)
result_kappa <- kappa_calc_2(p_vec1, p_vec2)
plot_kappa(result_kappa$tb_dat_ch)


