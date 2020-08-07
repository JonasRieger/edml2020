library(tosca)
library(ldaPrototype)

# sampled/chosen (change, if you want to have a look at different topics/dates):
t = 14
d = "2018-07-07"

topicsday = readRDS(file = file.path("topicsBW", "topicsday10.rds"))
topicsdayword10 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBW", "topicsday20.rds"))
topicsdayword20 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBW", "topicsday30.rds"))
topicsdayword30 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBW", "topicsday50.rds"))
topicsdayword50 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBW", "topicsday100.rds"))
topicsdayword100 = topicsday[,t,d] / rowSums(topicsday[,,d])

topicsday = readRDS(file = file.path("topicsBS", "topicsday10.rds"))
topicsday10 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBS", "topicsday20.rds"))
topicsday20 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBS", "topicsday30.rds"))
topicsday30 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBS", "topicsday50.rds"))
topicsday50 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBS", "topicsday100.rds"))
topicsday100 = topicsday[,t,d] / rowSums(topicsday[,,d])

topicsday = readRDS(file = file.path("topicsBSW", "topicsday10.rds"))
topicsdaywordsent10 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBSW", "topicsday20.rds"))
topicsdaywordsent20 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBSW", "topicsday30.rds"))
topicsdaywordsent30 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBSW", "topicsday50.rds"))
topicsdaywordsent50 = topicsday[,t,d] / rowSums(topicsday[,,d])
topicsday = readRDS(file = file.path("topicsBSW", "topicsday100.rds"))
topicsdaywordsent100 = topicsday[,t,d] / rowSums(topicsday[,,d])

protolda = getLDA(readRDS("SZ18proto.rds"))
docs = readRDS("SZ18docs.rds")
obj = readRDS("SZ18meta.rds")

count = plotTopic(obj, protolda, names(docs), unit = "day")
nday = sum(count[count$date == d,-1])
est = count[count$date == d, t+1]/nday

myfilterID = function(object, id){
  object$meta = object$meta[object$meta$id %in% id,]
  object
}

id50 = readRDS(file.path("sampleids", "id50.rds"))
count50 = plotTopic(myfilterID(obj, id50), protolda, names(docs), unit = "day")
nday50 = sum(count50[count50$date == d,-1])
est50 = count50[count50$date == d, t+1]/nday50

id30 = readRDS(file.path("sampleids", "id30.rds"))
count30 = plotTopic(myfilterID(obj, id30), protolda, names(docs), unit = "day")
nday30 = sum(count30[count30$date == d,-1])
est30 = count30[count30$date == d, t+1]/nday30

id20 = readRDS(file.path("sampleids", "id20.rds"))
count20 = plotTopic(myfilterID(obj, id20), protolda, names(docs), unit = "day")
nday20 = sum(count20[count20$date == d,-1])
est20 = count20[count20$date == d, t+1]/nday20

id10 = readRDS(file.path("sampleids", "id10.rds"))
count10 = plotTopic(myfilterID(obj, id10), protolda, names(docs), unit = "day")
nday10 = sum(count10[count10$date == d,-1])
est10 = count10[count10$date == d, t+1]/nday10

KI = function(p, n, alpha = 0.05) p + c(-1, 1) * qnorm(1-alpha/2) * sqrt(p*(1-p)/n)

par(mar = c(3.3,2.8,2.2,0), xpd = FALSE) # adjust!

col = RColorBrewer::brewer.pal(8, "Dark2")[5:8]

plot(1,1, xlab = "",
  ylab = "",
  ylim = c(0, 0.055),
  axes = FALSE, xlim = c(0.7, 5.3), type="n")
lines(c(0.7,0.7), KI(est10, nday10), col = "darkgrey")
points(0.7, est10, col = col[4], pch = 20)
lines(c(0.9,0.9), quantile(topicsdayword10, probs = c(0.025,0.975)), col = col[1])
points(0.9, mean(topicsdayword10), col = "grey", bg = col[1], pch = 21)
lines(c(1.1,1.1), quantile(topicsday10, probs = c(0.025,0.975)), col = col[2])
points(1.1, mean(topicsday10), col = "grey", bg = col[2], pch = 21)
lines(c(1.3,1.3), quantile(topicsdaywordsent10, probs = c(0.025,0.975)), col = col[3])
points(1.3, mean(topicsdaywordsent10), col = "grey", bg = col[3], pch = 21)

lines(c(0.7,0.7)+1, KI(est20, nday20), col = "darkgrey")
points(0.7+1, est20, col = col[4], pch = 20)
lines(c(0.9,0.9)+1, quantile(topicsdayword20, probs = c(0.025,0.975)), col = col[1])
points(0.9+1, mean(topicsdayword20), col = "grey", bg = col[1], pch = 21)
lines(c(1.1,1.1)+1, quantile(topicsday20, probs = c(0.025,0.975)), col = col[2])
points(1.1+1, mean(topicsday20), col = "grey", bg = col[2], pch = 21)
lines(c(1.3,1.3)+1, quantile(topicsdaywordsent20, probs = c(0.025,0.975)), col = col[3])
points(1.3+1, mean(topicsdaywordsent20), col = "grey", bg = col[3], pch = 21)

lines(c(0.7,0.7)+2, KI(est30, nday30), col = "darkgrey")
points(0.7+2, est30, col = col[4], pch = 20)
lines(c(0.9,0.9)+2, quantile(topicsdayword30, probs = c(0.025,0.975)), col = col[1])
points(0.9+2, mean(topicsdayword30), col = "grey", bg = col[1], pch = 21)
lines(c(1.1,1.1)+2, quantile(topicsday30, probs = c(0.025,0.975)), col = col[2])
points(1.1+2, mean(topicsday30), col = "grey", bg = col[2], pch = 21)
lines(c(1.3,1.3)+2, quantile(topicsdaywordsent30, probs = c(0.025,0.975)), col = col[3])
points(1.3+2, mean(topicsdaywordsent30), col = "grey", bg = col[3], pch = 21)

lines(c(0.7,0.7)+3, KI(est50, nday50), col = "darkgrey")
points(0.7+3, est50, col = col[4], pch = 20)
lines(c(0.9,0.9)+3, quantile(topicsdayword50, probs = c(0.025,0.975)), col = col[1])
points(0.9+3, mean(topicsdayword50), col = "grey", bg = col[1], pch = 21)
lines(c(1.1,1.1)+3, quantile(topicsday50, probs = c(0.025,0.975)), col = col[2])
points(1.1+3, mean(topicsday50), col = "grey", bg = col[2], pch = 21)
lines(c(1.3,1.3)+3, quantile(topicsdaywordsent50, probs = c(0.025,0.975)), col = col[3])
points(1.3+3, mean(topicsdaywordsent50), col = "grey", bg = col[3], pch = 21)

lines(c(0.7,0.7)+4, KI(est, nday), col = "darkgrey")
points(0.7+4, est, col = col[4], pch = 20)
lines(c(0.9,0.9)+4, quantile(topicsdayword100, probs = c(0.025,0.975)), col = col[1])
points(0.9+4, mean(topicsdayword100), col = "grey", bg = col[1], pch = 21)
lines(c(1.1,1.1)+4, quantile(topicsday100, probs = c(0.025,0.975)), col = col[2])
points(1.1+4, mean(topicsday100), col = "grey", bg = col[2], pch = 21)
lines(c(1.3,1.3)+4, quantile(topicsdaywordsent100, probs = c(0.025,0.975)), col = col[3])
points(1.3+4, mean(topicsdaywordsent100), col = "grey", bg = col[3], pch = 21)

mtext(expression(hat(p)), 2, 2.2, las = 2, cex = 0.9)
mtext("Sample Size", 1, 2.1, cex = 0.9)

axis(2)
axis(1, at = 1:5, labels = c("10%", "20%", "30%", "50%", "100%"), tick = FALSE)

abline(v = c(1.5,2.5,3.5,4.5), col = "grey", lty = 3)
par(mar = c(3.3,3.2,2,0), xpd = TRUE) # adjust!

# adjust!
legend("top", c("Analyt.", "BW", "BS", "BSW"), horiz = TRUE, inset = c(0,-0.22),
  col = c(col[4], rep("darkgrey", 3)), pt.bg = c(col[4], col[1:3]), pch = c(20,21,21,21))
