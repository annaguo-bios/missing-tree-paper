N <- c(500, 1000, 2000, 4000, 8000)
nsim <- 500

for (i in seq_along(N)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",N[i]," ",t) # 
    
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("joblist_n",i,".txt") ,quote = F, col.names = F, row.names = F)
}
