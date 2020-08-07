library(tosca)

docs = readRDS("SZ18docs.rds")
obj = readRDS("SZ18meta.rds")
tabelle = obj$meta[match(names(docs), obj$meta$id), c("id", "date")]
rm(docs, obj)
gc()

splitted = split(tabelle$id, tabelle$date)

id10 = character(0)
id20 = character(0)
id30 = character(0)
id50 = character(0)

set.seed(20200127)

for(i in seq_along(splitted)){
  nSam = length(splitted[[i]])
  
  tmpid50 = sample(splitted[[i]], nSam*0.5)
  tmpid30 = sample(tmpid50, nSam*0.3)
  tmpid20 = sample(tmpid30, nSam*0.2)
  tmpid10 = sample(tmpid20, nSam*0.1)
  
  id50 = c(id50, tmpid50)
  id30 = c(id30, tmpid30)
  id20 = c(id20, tmpid20)
  id10 = c(id10, tmpid10)
}

saveRDS(id50, file = file.path("sampleids", "id50.rds"))
saveRDS(id30, file = file.path("sampleids", "id30.rds"))
saveRDS(id20, file = file.path("sampleids", "id20.rds"))
saveRDS(id10, file = file.path("sampleids", "id10.rds"))
