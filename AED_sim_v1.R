library(tidyverse)
setwd("~/Desktop/AED_sim")
read.delim("AED_sim.txt",row.names=1) %>% as.data.frame -> ax

# rows: patients a-j
# columns: 10 time intervals with absent = 0, present = 1

dim(ax)[1] * dim(ax)[2] -> sz
sum(ax) -> pos
pos/sz -> freq
-log(freq,2) -> ic_pos
nfreq <- 1-freq
-log(nfreq,2) -> ic_neg

# compare patient a and b

subx <- ax[1:2,]
subx[3,] <- NA



#Assess probability at each time point

matchx <- function(x)
{
  if(x==0){y = ic_neg}
  if(x==1){y = ic_pos}
  return(y)
}

AED_sim <- function(a1,a2) 
{
  s1 <- ax[a1,]
  s2 <- ax[a2,]
  
  subx <- rbind(s1,s2)
  subx[3,] <- NA
  cc <- rownames(subx)
  cc[3] <- "sum"
  rownames(subx) <- cc
  
  for (i in 1:10)
  {
    subx[1,i] %>% matchx -> pat1
    subx[2,i] %>% matchx -> pat2
    pat1 + pat2 -> subx[3,i] 
  }
  
  sum(subx[3,]) %>% return
}

#create 10 x 10 matrix

patx <- rownames(ax)
ll <- length(patx)
sim_matrix <- matrix(ncol=ll,nrow=ll) %>% as.data.frame()
rownames(sim_matrix) <- patx
names(sim_matrix) <- patx

for (i in 1:10) # i rows
{
  for (k in 1:10) # k columns
  {
    sim_matrix[i,k] <- AED_sim(i,k)
    if (i==k) {sim_matrix[i,k] <- NA}
    paste("row",i,"column",k,"value",sim_matrix[i,k]) %>% print()
  }
}

sim_matrix %>% as.matrix %>% image(col = heat.colors(20))



