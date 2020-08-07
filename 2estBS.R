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

sapply(1:15, function(n) choose(2*n-1, n-1))
# up to and including 9 sentences: calculate all combinations (e.g. for 9: 24 310)
# more than 9: sample 25 000 combinations

# bootstrap study (off course takes some time)
# non-parallel version
if(FALSE){
  res = list()
  for(i in setdiff(seq_len(length(assignments)), singles)){
    splitted = split(assignments[[i]], sentences[[i]])
    nSaetze = nsentences[i]
    if(nSaetze < 10){
      tmp = t(sapply(apply(combinations(nSaetze, nSaetze, replace = TRUE), 1, function(x)
        unlist(splitted[x])+1), tabulate, nbins = 50))
      colnames(tmp) = 1:50
      res[[i]] = tmp[, colSums(tmp) > 0, drop = FALSE]
    }else{
      tmp = t(replicate(25000,
        tabulate(unlist(splitted[sample(nSaetze, nSaetze, replace = TRUE)])+1, nbins = 50)))
      colnames(tmp) = 1:50
      res[[i]] = tmp[,colSums(tmp) > 0, drop = FALSE]
    }
  }
  res[singles] = lapply(assignments[singles], function(x){
    tab = tabulate(x+1, nbins = 50)
    names(tab) = 1:50
    tab[tab != 0]
  })
}

# parallel version
library(parallelMap)
parallelStart("socket", cpus = 2)
parallelExport("assignments", "sentences", "nsentences", "combinations")
res = list()
set.seed(20191129)
res[setdiff(seq_len(length(assignments)), singles)] = parallelLapply(
  setdiff(seq_len(length(assignments)), singles), function(i){
    splitted = split(assignments[[i]], sentences[[i]])
    nSaetze = nsentences[i]
    if(nSaetze < 10){
      tmp = t(sapply(apply(combinations(nSaetze, nSaetze, replace = TRUE), 1, function(x)
        unlist(splitted[x])+1), tabulate, nbins = 50))
      colnames(tmp) = 1:50
      return(tmp[,colSums(tmp) > 0, drop = FALSE])
    }else{
      tmp = t(replicate(25000,
        tabulate(unlist(splitted[sample(nSaetze, nSaetze, replace = TRUE)])+1, nbins = 50)))
      colnames(tmp) = 1:50
      return(tmp[,colSums(tmp) > 0, drop = FALSE])
    }
  })
parallelStop()

res[singles] = lapply(assignments[singles], function(x){
  tab = tabulate(x+1, nbins = 50)
  names(tab) = 1:50
  tab[tab != 0]
})

saveRDS(res, file = "resBS.rds")

# computation of estimators based on the bootstrap study result (takes several hours)
est = lapply(res[singles], function(x){
  x = c("0"=0, x)
  (x+alpha) / (sum(x)+K*alpha)
})

dir.create("estBS")
saveRDS(est, file = file.path("estBS", "1.rds"))
scand = sort(unique(nsentences))[-1]
lapply(scand, function(y){
  tmp = res[nsentences == y]
  tmp = lapply(tmp, function(x){
    x = cbind("0"=0, x)
    (x+alpha) / (rowSums(x)+K*alpha)
  })
  saveRDS(tmp, file = file.path("estBS", paste0(y, ".rds")))
})
