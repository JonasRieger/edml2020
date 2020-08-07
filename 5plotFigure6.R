par(mar = c(3.3,4.5,0.8,2), mfrow = c(3,1)) # adjust!

set.seed(20200205)
x = readRDS(file.path("coefficientvariation", "xBW.rds"))
i = sample(seq_along(x), length(x)*0.2)
# sample 20% for visualization

for (method in c("BW", "BS", "BSW")){
  x = readRDS(file.path("coefficientvariation", paste0("x", method, ".rds")))
  y = readRDS(file.path("coefficientvariation", paste0("y", method, ".rds")))
  
  col = c("black", RColorBrewer::brewer.pal(4, "Dark2"))
  col = rep(col, each = length(x)/5)
  
  plot(x, y, xlab = "", ylab = "",
    ylim = c(0, 0.0081), type = "n", xlim = c(0,0.15), cex.axis = 1.2)
  points(x[i], y[i], col = col[i])
  
  mtext(expression(SD(hat(p))), 2, 2.5, cex = 0.9)
  mtext(expression(hat(p)), 1, 2, cex = 0.9)
  tmp = switch(method,
    BS = "BSentence",
    BW = "BWord",
    BSW = "BSentenceWord"
  )
  mtext(tmp, 4, 0.4, cex = 0.9)
  #pos = switch(method,
  #  BS = "bottomright",
  #  BW = "topleft",
  #  BSW = "bottomright"
  #)
  if(method == "BW"){
    legend("topleft", c("  10%", "  20%", "  30%", "  50%", "100%"),
      col = rev(c("black", RColorBrewer::brewer.pal(4, "Dark2"))), pch = 20, cex = 1.4, horiz = TRUE)
  }
  #legend(pos, c("  10%", "  20%", "  30%", "  50%", "100%"),
  #  col = rev(c("black", RColorBrewer::brewer.pal(4, "Dark2"))), pch = 20, cex = 1.3, horiz = TRUE)
}
