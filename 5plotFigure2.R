library(ldaPrototype)
library(tosca)

# sampled/chosen (change, if you want to have a look at different documents):
nsent = 32
nr = 550

estsent = readRDS(file.path("estBS", paste0(nsent, ".rds")))
estword = readRDS(file.path("estBW", paste0(nsent, ".rds")))
estwordsent = readRDS(file.path("estBSW", paste0(nsent, ".rds")))
assignments = readRDS(file.path("assignments", paste0(nsent, ".rds")))

protolda = getLDA(readRDS("SZ18proto.rds"))
K = getK(protolda)
alpha = getAlpha(protolda)
theta = getEstimators(protolda)$theta

sentences = readRDS("SZ18sentences.rds")
nsentences = sapply(sentences, function(x) length(unique(x)))

id = names(which(nsentences == nsent))[nr]

docs = readRDS("SZ18docs.rds")
est = theta[,match(id, names(docs))]
est = est[c(setdiff(1:K, as.integer(colnames(estsent[[nr]])[-1]))[1], as.integer(colnames(estsent[[nr]])[-1]))]
topWords(getTopics(protolda), 5)[,as.integer(colnames(estsent[[nr]])[-1])]
nwords = length(getAssignments(protolda)[[match(id, names(docs))]])

ploty = function(est, nwords, sent, word, wordsent, nr){
  
  col = RColorBrewer::brewer.pal(8, "Dark2")[5:8]
  
  KI = function(p, n, alpha = 0.05) p + c(-1, 1) * qnorm(1-alpha/2) * sqrt(p*(1-p)/n)
  ki = sapply(seq_along(est), function(i) KI(est[i], nwords))
  
  sent = sent[[nr]]
  word = word[[nr]]
  wordsent = wordsent[[nr]]
  sentq = apply(sent, 2, quantile, probs = c(0.025, 0.5, 0.975))
  wordq = apply(word, 2, quantile, probs = c(0.025, 0.5, 0.975))
  wordsentq = apply(wordsent, 2, quantile, probs = c(0.025, 0.5, 0.975))
  
  plot(1,1, xlab = "", ylab = "", pch = 20,
    ylim = c(0, max(c(ki, wordq, sentq, wordsentq))),
    axes = FALSE, xlim = c(0.5, ncol(sentq)+0.5), type="n")
  
  lapply(1:ncol(wordq), function(y) lines(c(y- 0.1, y- 0.1), wordq[c("2.5%", "97.5%"),y], col = col[1]))
  points(1:ncol(wordq)-0.1, apply(sent, 2, mean), col = "grey", bg = col[1], pch = 21)
  
  lapply(1:ncol(sentq), function(y) lines(c(y+ 0.1, y+ 0.1), sentq[c("2.5%", "97.5%"),y], col = col[2]))
  points(1:ncol(sentq)+0.1, apply(word, 2, mean), col = "grey", bg = col[2], pch = 21)
  
  lapply(1:ncol(wordsentq), function(y) lines(c(y+0.3, y+0.3), wordsentq[c("2.5%", "97.5%"),y], col = col[3]))
  points(1:ncol(wordsentq)+0.3, apply(wordsent, 2, mean), col = "grey", bg = col[3], pch = 21)
  
  for(i in seq_len(ncol(ki))) lines(c(i-0.3, i-0.3), ki[,i], col = "darkgrey")
  points(seq_along(est)-0.3, est, col = col[4], pch = 20)
  
  axis(2)
  mtext(expression(hat(theta)),2, 2.2, las = 2)
  mtext("Topic", 1, 2.2)
  axis(1, at = 1:ncol(sentq), labels = colnames(sentq))
  legend("top", c("Analytically", "BWord", "BSentence", "BSentenceWord"), horiz = TRUE, inset = c(0,-0.2),
    col = c(col[4], rep("darkgrey", 3)), pt.bg = c(col[4], col[1:3]), pch = c(20,21,21,21))
}


ploty2 = function(est, nwords, sent, word, wordsent, nr){
  
  col = RColorBrewer::brewer.pal(8, "Dark2")[5:8]
  
  KI = function(p, n, alpha = 0.05) p + c(-1, 1) * qnorm(1-alpha/2) * sqrt(p*(1-p)/n)
  ki = sapply(seq_along(est), function(i) KI(est[i], nwords))
  
  sent = sent[[nr]]
  word = word[[nr]]
  wordsent = wordsent[[nr]]
  sentq = apply(sent, 2, quantile, probs = c(0.025, 0.5, 0.975))
  wordq = apply(word, 2, quantile, probs = c(0.025, 0.5, 0.975))
  wordsentq = apply(wordsent, 2, quantile, probs = c(0.025, 0.5, 0.975))
  
  plot(1,1, xlab = "", ylab = "", pch = 20,
    ylim = c(0, max(c(ki, wordq, sentq, wordsentq))),
    axes = FALSE, xlim = c(0.5, ncol(sentq)+0.5), type="n")
  
  lapply(1:ncol(wordq), function(y) lines(c(y- 0.1, y- 0.1), wordq[c("2.5%", "97.5%"),y], col = col[1]))
  points(1:ncol(wordq)-0.1, apply(sent, 2, mean), col = "grey", bg = col[1], pch = 21)
  
  lapply(1:ncol(sentq), function(y) lines(c(y+ 0.1, y+ 0.1), sentq[c("2.5%", "97.5%"),y], col = col[2]))
  points(1:ncol(sentq)+0.1, apply(word, 2, mean), col = "grey", bg = col[2], pch = 21)
  
  lapply(1:ncol(wordsentq), function(y) lines(c(y+0.3, y+0.3), wordsentq[c("2.5%", "97.5%"),y], col = col[3]))
  points(1:ncol(wordsentq)+0.3, apply(wordsent, 2, mean), col = "grey", bg = col[3], pch = 21)
  
  for(i in seq_len(ncol(ki))) lines(c(i-0.3, i-0.3), ki[,i], col = "darkgrey")
  points(seq_along(est)-0.3, est, col = col[4], pch = 20)
  
  axis(2)
  mtext(expression(hat(theta)),2, 2.2, las = 2)
  axis(1, at = 1:ncol(sentq), labels = rep("", 15))
  text(1:15, -0.04, labels = c("Other Topics", "Police, Crimes", "Asia, esp. China", "Stopwords, Education", "FIFA WM 2018", "Migration", "Government", "Stopwords",
    "History, Literature", "Parties", "USA, Turkey", "Automotive Industry", "Russia", "EU, Brexit", "Syria, Isreal"),
    srt = 30, cex = 0.8, adj = 1) # adjust!
  # change labels for different texts (nsent, nr)!
  legend("top", c("Analytically", "BWord", "BSentence", "BSentenceWord"), horiz = TRUE, inset = c(0,-0.23),
    col = c(col[4], rep("darkgrey", 3)), pt.bg = c(col[4], col[1:3]), pch = c(20,21,21,21))
}

#par(mar = c(3.5,3,2,0), xpd = TRUE)
#ploty(est, nwords, estsent, estword, estword, nr)
#text(1:15, c(0.02,0.17,0.06,0.06,0.02,0,0.08,0.02,0.1,0,0.1,0.04,0.1,0,0.02),
#  labels = c("Other Topics", "Police, Crimes", "Asia, esp. China", "Stopwords, Education", "FIFA WM 2018", "Migration", "Government", "Stopwords",
#  "History, Literature", "Parties", "USA, Turkey", "Automotive Industry", "Russia", "EU, Brexit", "Syria, Isreal"),
#  srt = 90, cex = 0.6, adj = 0)

par(mar = c(7,3,1.9,0), xpd = TRUE) # adjust!
ploty2(est, nwords, estsent, estword, estword, nr)
