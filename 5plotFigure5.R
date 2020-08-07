roll = readRDS("roll.rds")

ploty = function(l, type, per, ymax){
  l = l[[type]][[per]]
  if(missing(ymax)) ymax = max(c(l[["7"]], l[["15"]], l[["29"]]))
  
  plot(seq.Date(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by = "day"), rep(0,365),
    xlim = c(), ylim = c(0, ymax), type = "n", xlab = "", ylab = "", cex.axis = 1.1)
  
  polygon(c(as.Date(rownames(l[["7"]])), rev(as.Date(rownames(l[["7"]])))),
    c(l[["7"]][,1], rev(l[["7"]][,3])), col = "grey", border = NA)
  points(as.Date(rownames(l[["7"]])), l[["7"]][,2], type = "l", col = "black")
  
  polygon(c(as.Date(rownames(l[["15"]])), rev(as.Date(rownames(l[["15"]])))),
    c(l[["15"]][,1], rev(l[["15"]][,3])), col = "steelblue2", border = NA)
  points(as.Date(rownames(l[["15"]])), l[["15"]][,2], type = "l", col = "blue")
  
  polygon(c(as.Date(rownames(l[["29"]])), rev(as.Date(rownames(l[["29"]])))),
    c(l[["29"]][,1], rev(l[["29"]][,3])), col = "lightpink", border = NA)
  points(as.Date(rownames(l[["29"]])), l[["29"]][,2], type = "l", col = "red")
  
  typetmp = switch (type,
    BW = "BWord",
    BS = "BSentence",
    BSW = "BSentenceWord"
  )
  
  mtext(paste0(typetmp, ": ", per, "%"), 3, 0.1, adj = 0, cex = 0.85)
  mtext(expression(hat(p)), 2, 2.2, las = 2, cex = 0.85)
  mtext("2018", 1, 2, cex = 0.85)
}

par(mar = c(3.3,3.3,1.5,1), mfrow = c(3,2)) # adjust!

# select scenarios as you like
ploty(roll, "BW", "10", ymax = 0.06)
ploty(roll, "BSW", "20", ymax = 0.035)
ploty(roll, "BS", "10", ymax = 0.06)
ploty(roll, "BSW", "50", ymax = 0.035)
ploty(roll, "BSW", "10", ymax = 0.06)
ploty(roll, "BSW", "100", ymax = 0.035)
