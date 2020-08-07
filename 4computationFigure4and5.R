library(lubridate)
library(zoo)

for(method in c("BS", "BW", "BSW")){
  for(sam in c("10", "20", "30", "50", "100")){
    topicsday = readRDS(file = file.path(paste0("topics", method), paste0("topicsday", sam, ".rds")))
    tmp = array(0L, dim = c(100000, 2, 365),
      dimnames = list(paste0("Rep", seq_len(100000)), paste0("Aggr", 1:2), as.character(seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), "day"))))
    tmp[,1,] = topicsday[,14,]
    tmp[,2,] = apply(topicsday, 3, rowSums)
    rm(topicsday)
    gc()
    for(window in c(7,15,29)){
      roll = array(0L, dim = c(100000, 2, 365-window+1),
        dimnames = list(paste0("Rep", seq_len(100000)), paste0("Aggr", 1:2), as.character(seq.Date(as.Date("2018-01-01")+floor(window/2), as.Date("2018-12-31")-floor(window/2), "day"))))
      roll[,1,] = t(rollapply(t(tmp[,1,]), width = window, by = 1, FUN = mean))
      roll[,2,] = t(rollapply(t(tmp[,2,]), width = window, by = 1, FUN = mean))
      saveRDS(roll[,1,]/roll[,2,], file = file.path(paste0("topics", method), paste0("topics", sam, "roll", window, ".rds")))
    }
  }
}

# summarize (2,5% and 97,5% quantile and means) in one object
res = list()
for(method in c("BS", "BW", "BSW")){
  for(sam in c("10", "20", "30", "50", "")){
    restmp = list()
    for(window in c(7,15,29)){
      topicsday = readRDS(file = file.path(paste0("topics", method), paste0("topics", sam, "roll", window, ".rds")))
      restmp[[as.character(window)]] = cbind(
        apply(topicsday, 2, quantile, 0.025),
        colMeans(topicsday),
        apply(topicsday, 2, quantile, 0.975))
    }
    res[[method]][[sam]] = restmp
  }
}
saveRDS(res, file = "roll.rds")
