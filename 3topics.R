library(tosca)
library(lubridate)

docs = readRDS("SZ18docs.rds")
obj = readRDS("SZ18meta.rds")
tabelle = obj$meta[match(names(docs), obj$meta$id), c("id", "date")]
rm(docs, obj)
gc()

tabelle$id100 = TRUE
tabelle$id50 = tabelle$id %in% readRDS(file.path("sampleids", "id50.rds"))
tabelle$id30 = tabelle$id %in% readRDS(file.path("sampleids", "id30.rds"))
tabelle$id20 = tabelle$id %in% readRDS(file.path("sampleids", "id20.rds"))
tabelle$id10 = tabelle$id %in% readRDS(file.path("sampleids", "id10.rds"))

for(type in c("BS", "BW", "BSW")){
  # sorting in res is the same as in docs
  res = readRDS(paste0("res", type, ".rds"))
  for(sam in c(100,50,30,20,10)){
    ind = tabelle[,paste0("id", sam)]
    dates = tabelle$date[ind]
    res = res[ind]
    tabelle = tabelle[ind,]
    
    weeks = floor_date(dates, "week")
    months = floor_date(dates, "month")
    
    n = 100000
    topicsday = array(0L, dim = c(n, 50, 365),
      dimnames = list(paste0("Rep", seq_len(n)), paste0("Topic", 1:50), as.character(seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), "day"))))
    topicsweek = array(0L, dim = c(n, 50, length(unique(weeks))),
      dimnames = list(paste0("Rep", seq_len(n)), paste0("Topic", 1:50), as.character(sort(unique(weeks)))))
    topicsmonth = array(0L, dim = c(n, 50, length(unique(months))),
      dimnames = list(paste0("Rep", seq_len(n)), paste0("Topic", 1:50), as.character(sort(unique(months)))))
    topicsyear = matrix(0L, nrow = n, ncol = 50)
    
    set.seed(20200115)
    j = 0
    for(tab in res){
      j = j+1
      date = as.character(dates[j])
      week = as.character(weeks[j])
      month = as.character(months[j])
      if(!is.null(nrow(tab))){
        topics = as.integer(colnames(tab))
        value = tab[sample(seq_len(nrow(tab)), n, replace = TRUE),]
      }else{
        topics = as.integer(names(tab))
        value = t(replicate(n, tab))
      }
      topicsday[, topics, date] = topicsday[, topics, date] + value
      topicsweek[, topics, week] = topicsweek[, topics, week] + value
      topicsmonth[, topics, month] = topicsmonth[, topics, month] + value
      topicsyear[, topics] = topicsyear[, topics] + value
    }
    
    saveRDS(topicsday, file = file.path(paste0("topics", type), paste0("topicsday", sam, ".rds")))
    saveRDS(topicsweek, file = file.path(paste0("topics", type), paste0("topicsweek", sam, ".rds")))
    saveRDS(topicsmonth, file = file.path(paste0("topics", type), paste0("topicsmonth", sam, ".rds")))
    saveRDS(topicsyear, file = file.path(paste0("topics", type), paste0("topicsyear", sam, ".rds")))
  }
}
