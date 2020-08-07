for (method in c("BS", "BW", "BSW")){
  topicsday = readRDS(file = file.path(paste0("topics", method), "topicsday10.rds"))
  for(i in seq_len(dim(topicsday)[3])){
    topicsday[,,i] = topicsday[,,i]/rowSums(topicsday[,,i])
  }
  mu10 = sapply(1:50, function(t) apply(topicsday[,t,], 2, mean))
  serr10 = sapply(1:50, function(t) apply(topicsday[,t,], 2, sd))
  topicsday = readRDS(file = file.path(paste0("topics", method), "topicsday20.rds"))
  for(i in seq_len(dim(topicsday)[3])){
    topicsday[,,i] = topicsday[,,i]/rowSums(topicsday[,,i])
  }
  mu20 = sapply(1:50, function(t) apply(topicsday[,t,], 2, mean))
  serr20 = sapply(1:50, function(t) apply(topicsday[,t,], 2, sd))
  topicsday = readRDS(file = file.path(paste0("topics", method), "topicsday30.rds"))
  for(i in seq_len(dim(topicsday)[3])){
    topicsday[,,i] = topicsday[,,i]/rowSums(topicsday[,,i])
  }
  mu30 = sapply(1:50, function(t) apply(topicsday[,t,], 2, mean))
  serr30 = sapply(1:50, function(t) apply(topicsday[,t,], 2, sd))
  topicsday = readRDS(file = file.path(paste0("topics", method), "topicsday50.rds"))
  for(i in seq_len(dim(topicsday)[3])){
    topicsday[,,i] = topicsday[,,i]/rowSums(topicsday[,,i])
  }
  mu50 = sapply(1:50, function(t) apply(topicsday[,t,], 2, mean))
  serr50 = sapply(1:50, function(t) apply(topicsday[,t,], 2, sd))
  topicsday = readRDS(file = file.path(paste0("topics", method), "topicsday100.rds"))
  for(i in seq_len(dim(topicsday)[3])){
    topicsday[,,i] = topicsday[,,i]/rowSums(topicsday[,,i])
  }
  mu100 = sapply(1:50, function(t) apply(topicsday[,t,], 2, mean))
  serr100 = sapply(1:50, function(t) apply(topicsday[,t,], 2, sd))
  
  x = c(mu100, mu50, mu30, mu20, mu10)
  y = c(serr100, serr50, serr30, serr20, serr10)
  
  saveRDS(x, file = file.path("coefficientvariation", paste0("x", method, ".rds")))
  saveRDS(y, file = file.path("coefficientvariation", paste0("y", method, ".rds")))
}
