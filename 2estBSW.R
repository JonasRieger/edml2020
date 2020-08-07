library(tosca)
library(ldaPrototype)
library(arrangements)

proto = readRDS("SZ18proto.rds")
sentences = readRDS("SZ18sentences.rds")
nsentences = sapply(sentences, function(x) length(unique(x)))
singles = which(nsentences == 1)

lda = getLDA(proto)
assignments = getAssignments(lda)
eta = getEta(lda)
alpha = getAlpha(lda)
K = getK(lda)
L = ncol(getTopics(lda))

library(parallelMap)
parallelStart("socket", cpus = 8)
parallelExport("assignments", "sentences", "nsentences", "combinations")
res = list()
set.seed(20191129)
res[setdiff(seq_len(length(assignments)), singles)] = parallelLapply(
  setdiff(seq_len(length(assignments)), singles), function(i){
    splitted = split(assignments[[i]], sentences[[i]])
    nSaetze = nsentences[i]
    tmp = t(replicate(25000,
      tabulate(unlist(
        lapply(splitted[sample(nSaetze, nSaetze, replace = TRUE)],
          function(x) sample(x, length(x), replace = TRUE)
        ))
        +1, nbins = 50)))
    colnames(tmp) = 1:50
    return(tmp[,colSums(tmp) > 0, drop = FALSE])
  })
parallelStop()

res[singles] = lapply(assignments[singles], function(i){
  if(length(i) < 10){
    tmp = t(sapply(apply(combinations(length(i), length(i), replace = TRUE), 1, function(x)
      unlist(i[x])+1), tabulate, nbins = 50))
  }else{
    tmp = t(replicate(25000,
      tabulate(unlist(i[sample(length(i), length(i), replace = TRUE)])+1, nbins = 50)))
  }
  colnames(tmp) = 1:50
  return(tmp[,colSums(tmp) > 0, drop = FALSE])
})

saveRDS(res, file = "resBSW.rds")

dir.create("estBSW")
scand = sort(unique(nsentences))
for(y in scand){
  tmp = res[nsentences == y]
  tmp = lapply(tmp, function(x){
    x = cbind("0"=0, x)
    (x+alpha) / (rowSums(x)+K*alpha)
  })
  saveRDS(tmp, file = file.path("estBSW", paste0(y, ".rds")))
}
