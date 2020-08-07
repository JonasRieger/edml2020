roll = readRDS("roll.rds")

ploty = function(l, type, per, ymax){
  l = l[[type]][[per]]
  if(missing(ymax)) ymax = max(c(l[["7"]], l[["15"]], l[["29"]]))
  
  x = seq.Date(from = as.Date("2018-06-01"), to = as.Date("2018-07-31"), by = "day")
  # change date sequence to have a look at other time intervals
  
  plot(x, rep(0,length(x)),
    xlim = c(), ylim = c(0, ymax), type = "n", xlab = "", ylab = "", cex.axis = 0.9)
  
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
  
  mtext(paste0(typetmp, ": ", per, "%"), 3, 0.1, adj = 0, cex = 0.9)
  mtext(expression(hat(p)), 2, 2.2, las = 2, cex = 0.9)
  mtext("2018", 1, 2, cex = 0.9)
}

par(mar = c(3.2,3,1.2,1), mfrow = c(1,1)) # adjust!

# change "BSW" and "30" to select other scenarios
ploty(roll, "BSW", "30")
legend(as.Date("2018-06-01"), 0.008, title = "Rolling Window", c("7 Days", "15 Days", "29 Days"), horiz = TRUE,
  col = c("black", "blue", "red"), lty = 1, box.col = "white", cex = 0.9)
