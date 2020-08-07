devtools::install_github("DoCMA-TU/tmT")
library(tmT)
library(tosca)
library(ldaPrototype)

# read data
SZ = readSZ() # XML files cannot be made available for legal reasons

# raw preprocessing
SZ$meta$title = removeHTML(SZ$meta$title, hex = FALSE, symbols = TRUE)
SZ$meta$title = removeUmlauts(SZ$meta$title)
SZ$text = removeHTML(SZ$text, hex = FALSE, symbols = TRUE)
SZ$text = removeUmlauts(SZ$text)

# duplicate removal
SZduplist = duplist(SZ, paragraph = TRUE)
SZ = filterID(SZ, id = SZduplist$uniqueTexts)

# filtering to articles in 2018
SZ = filterDate(SZ, s.date = as.Date("2018-01-01"), e.date = as.Date("2018-12-31"))
saveRDS(SZ, "SZ18.rds") # SZ18 cannot be made available for legal reasons
# instead, save selected meta data of texts
saveRDS(textmeta(meta = obj$meta[, c("id", "date", "title")]), "SZ18meta.rds")

# splitting texts by sentence borders
SZ$text = lapply(SZ$text,
  function(x)
    unlist(strsplit(tm::stripWhitespace(paste0(unlist(x), collapse = " ")), "\\.")))

# preprocessing as stopword removal and tokenization
SZ = cleanTexts(SZ, sw = "de", paragraph = TRUE)

# determination of vocabulary
wl = makeWordlist(SZ$text)
vocab = wl$words[wl$wordtable > 10]
saveRDS(vocab, "SZ18vocab.rds")

# deletion of non-vocabulary words (takes several hours)
SZ$text = lapply(SZ$text, function(x) lapply(x, function(y) y[y %in% vocab]))
# deletion of empty texts
SZ$text = SZ$text[lengths(lapply(SZ$text, unlist)) > 1]
SZ = filterDate(SZ)
saveRDS(SZ, "SZ18clean.rds") # SZ18clean cannot be made available for legal reasons

# allocate token to sentences
sentences = lapply(SZ$text, function(x){tmp = sapply(x, length); rep(seq_along(tmp), times = tmp)})
saveRDS(sentences, "SZ18sentences.rds")

# prepare corpus for LDA modeling
docs = LDAprep(SZ$text, vocab)
saveRDS(docs, "SZ18docs.rds")

# check
all(sapply(docs, ncol) == lengths(sentences)) # TRUE

# computation of the prototype LDA (takes several hours)
proto = LDAPrototype(docs, vocab, n = 100, pm.backend = "socket", ncpus = 4, K = 50)
saveRDS(proto, "SZ18proto.rds")

# save assignments per number of sentence in texts
nsentences = sapply(sentences, function(x) length(unique(x)))
assignments = getAssignments(getLDA(proto))
dir.create("assignments")
for(i in sort(unique(nsentences))){
  saveRDS(assignments[nsentences == i], file = file.path("assignments", paste0(i, ".rds")))
}
