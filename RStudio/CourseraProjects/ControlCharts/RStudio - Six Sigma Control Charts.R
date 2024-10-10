# 7 Control Charts
# IMR (XMR) Charts, XbarR-S Charts, P-NP Charts, C-U Charts
# Following the 4 Western Electric Rules
# Emilio Tinajero Montes

#Load Libraries
check = c("data.table", "nortest") %in% rownames(installed.packages())
if(!check[1]) install.packages("data.table")
if(!check[2]) install.packages("nortest")
library(data.table); library(nortest); rm(check)

# Import Control Chart Constants (d2,d3,c4,A2,D3,D4,B3,B4)
ccc = fread("http://pastebin.com/raw/qu2Z56Fa")

# Control Chart formulae at https://ibb.co/7VFNJ9K, alternatively - https://pasteboard.co/JCU5NJj.jpg

# I-MR Chart for I (input: time ordered vector)
XMRChartX <- function(x, label = "x"){
  n = length(x)
  mr = abs(x[2:n] - x[1:(n-1)])
  temp = data.table(index = 1:(n-1), x = x[2:n],mr)
  avgx = mean(x[2:n]); avgmr = mean(mr)
  Xlcl = avgx - (3*avgmr/ccc$d2[1])  # LCL = Xbar - 3*MRbar / d2
  Xucl = avgx + (3*avgmr/ccc$d2[1])  # LCL = Xbar + 3*MRbar / d2
  deltaz = avgmr/ccc$d2[1]
  # Plot Charts
  yupper = avgx + 1.2*(max(temp$x, Xucl) - avgx)
  ylower = avgx - 1.2*(avgx - min(temp$x, Xlcl))
  plot(temp$x, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("Individuals Chart (X-MR) of", label))
  lines(seq(1,n-1), rep(avgx,n-1), col = "darkgreen", lwd = 2 )
  lines(seq(1,n-1), rep(Xlcl,n-1), col = "red", lwd = 2 )
  lines(seq(1,n-1), rep(Xucl,n-1), col = "red", lwd = 2 ) 
  # Create Data for Tests
  temp = data.table(Points = x, CL = avgx, LCL = Xlcl, UCL = Xucl)
  temp = TestControlChart(temp)
  return(temp)
}

# I-MR Chart for MR (input: time ordered vector)
XMRChartMR <- function(x, label = "x"){
  n = length(x)
  mr = abs(x[2:n] - x[1:(n-1)])
  temp = data.table(x = x[2:n],mr)
  avgmr = mean(mr)
  MRucl = avgmr * ccc$D4[1]
  # Plot Charts
  yupper = avgmr + 1.2*(max(temp$mr, MRucl) - avgmr)
  ylower = avgmr - 1.2*(avgmr - min(temp$mr))
  plot(temp$mr, type = "o", pch = 20, lwd = 2, ylim = c(0, max(4*avgmr,1.2*max(mr))), ylab = label, main = paste("Moving Range Chart (X-MR) of", label))
  lines(seq(1,n-1), rep(avgmr,n-1), col = "darkgreen", lwd = 2 )
  lines(seq(1,n-1), rep(MRucl,n-1), col = "red", lwd = 2 )
  lines(seq(1,n-1), rep(0,n-1), col = "red", lwd = 2 )
  # Create Data for Tests
  temp[, Points := mr]
  temp[, CL := avgmr]
  temp[, LCL := 0]
  temp[, UCL := MRucl]
  temp = TestControlChart(temp)
  return(temp)
}

# Xbar-R Chart for Xbar (input: time ordered vector)
XbarRChartX <- function(x, g, label = "x", group = "g"){
  if(length(x) != length(g)) {
    return(cat("\n", "Input vectors have different lengths."))
  }
  temp = data.table(x = x, g = g)
  temp = temp[, .(.N, Xbar = mean(x), Range = (max(x)-min(x))), by = g]
  n = nrow(temp)
  avgXbar = mean(temp$Xbar); avgRange = mean(temp$Range)
  temp[, LCL := avgXbar -  ccc$A2[temp$N] * avgRange]
  temp[, UCL := avgXbar +  ccc$A2[temp$N] * avgRange]
  # Plot Charts
  yupper = avgXbar + 1.2*(max(temp$Xbar, temp$UCL) - avgXbar)
  ylower = avgXbar - 1.2*(avgXbar - min(temp$Xbar, temp$LCL))
  plot(temp$Xbar, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("Xbar Chart (Xbar-R) of", label, "by", group))
  lines(seq(1,n), rep(avgXbar,n), col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := Xbar]
  temp[, CL := avgXbar]
  temp = TestControlChart(temp)
  return(temp)
}

# Xbar-R Chart for R (input: time ordered vector)
XbarRChartR <- function(x, g, label = "x", group = "g"){
  if(length(x) != length(g)) {
    return(cat("\n", "Input vectors have different lengths."))
  }
  temp = data.table(x = x, g = g)
  temp = temp[, .(.N, Xbar = mean(x), Range = (max(x)-min(x))), by = g]
  n = nrow(temp)
  avgXbar = mean(temp$Xbar); avgRange = mean(temp$Range)
  temp[, LCL := ccc$D3[temp$N]*avgRange]
  temp[, UCL := ccc$D4[temp$N]*avgRange]
  # Plot Charts
  yupper = avgRange + 1.2*(max(temp$Range, temp$UCL) - avgRange)
  ylower = avgRange - 1.2*(avgRange - min(temp$Range, temp$LCL))
  plot(temp$Range, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("R Chart (Xbar-R) of", label, "by", group))
  lines(seq(1,n), rep(avgRange,n), col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := Range]
  temp[, CL := avgRange]
  temp = TestControlChart(temp)
  return(temp)
}

# Xbar-S Chart for Xbar (input: time ordered vector)
XbarSChartX <- function(x, g, label = "x", group = "g"){
  if(length(x) != length(g)) {
    return(cat("\n", "Input vectors have different lengths."))
  }
  temp = data.table(x = x, g = g)
  temp = temp[, .(.N, Xbar = mean(x), S = sd(x)), by = g]
  temp = temp[, S := ifelse(N == 1, 0,S)]
  n = nrow(temp)
  avgXbar = mean(temp$Xbar); avgS = mean(temp$S)
  vLCL = avgXbar -  ccc$A3[temp$N] * avgS
  vUCL = avgXbar +  ccc$A3[temp$N] * avgS
  temp[, LCL := vLCL]
  temp[, UCL := vUCL]
  # Plot Charts
  yupper = avgXbar + 1.2*(max(temp$Xbar, temp$UCL) - avgXbar)
  ylower = avgXbar - 1.2*(avgXbar - min(temp$Xbar, temp$LCL))
  plot(temp$Xbar, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("Xbar Chart (Xbar-S) of", label, "by", group))
  lines(seq(1,n), rep(avgXbar,n), col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := Xbar]
  temp[, CL := avgXbar]
  temp = TestControlChart(temp)
  return(temp)
}

# Xbar-S Chart for S (input: time ordered vector)
XbarSChartS <- function(x, g, label = "x", group = "g"){
  if(length(x) != length(g)) {
    return(cat("\n", "Input vectors have different lengths."))
  }
  temp = data.table(x = x, g = g)
  temp = temp[, .(.N, Xbar = mean(x), S = sd(x)), by = g]
  temp = temp[, S := ifelse(N == 1, 0,S)]
  n = nrow(temp)
  avgXbar = mean(temp$Xbar); avgS = mean(temp$S)
  temp[, LCL := ccc$B3[temp$N]*avgS]
  temp[, UCL := ccc$B4[temp$N]*avgS]
  # Plot Charts
  yupper = avgS + 1.2*(max(temp$S, temp$UCL) - avgS)
  ylower = avgS - 1.2*(avgS - min(temp$S, temp$LCL))
  plot(temp$S, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("S Chart (Xbar-S) of", label, "by", group))
  lines(seq(1,n), rep(avgS,n), col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := S]
  temp[, CL := avgS]
  temp = TestControlChart(temp)
  return(temp)
}

# P-Chart (input: Samples, Defects)
pChart <- function(nn, d, label = "x"){
  if(length(nn) != length(d)) {
    return(cat("\n", "Input vectors have different lengths."))
  }
  if(length(which(d/nn > 1)) > 0) {
    cat("\n", "Error: more defects than samples:")
    return(data.table(Sample = nn[which(d/nn > 1)], Defects = d[which(d/nn > 1)]))
  }
  temp = data.table(Sample = nn, Defects = d, p = d/nn)
  n = nrow(temp); pavg = mean(temp$p)
  temp[, CL := pavg]
  temp[, LCL := pavg - 3*sqrt(pavg*(1-pavg)/Sample)]
  temp[, UCL := pavg + 3*sqrt(pavg*(1-pavg)/Sample)]
  # Plot Charts
  yupper = pavg + 1.2*(max(temp$p, temp$UCL) - pavg)
  ylower = pavg - 1.2*(pavg - min(temp$p, temp$LCL))
  plot(temp$p, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("P-Chart of", label))
  lines(seq(1,n), rep(pavg,n), col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := p]
  temp = TestControlChart(temp)
  return(temp)
}

# NP-Chart (input: Num Samples (Const), Defects)
npChart <- function(nn, d, label = "x"){
  if(max(d) > nn) {
    cat("\n", "Error: more defects than Sample Size: ","\n")
    return(d[d > nn])
  }
  temp = data.table(NP = d, P = d/nn)
  n = length(d); pavg = mean(temp$P)
  temp[, CL := nn*pavg]
  temp[, LCL := nn*pavg - 3*sqrt(nn*pavg*(1-pavg))]
  temp[, UCL := nn*pavg + 3*sqrt(nn*pavg*(1-pavg))]
  # Plot Charts
  yupper = nn*pavg + 1.2*(max(temp$NP, temp$UCL) - nn*pavg)
  ylower = nn*pavg - 1.2*(nn*pavg - min(temp$NP, temp$LCL))
  plot(temp$NP, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("NP-Chart of", label))
  lines(seq(1,n), temp$CL, col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := NP]
  temp = TestControlChart(temp)
  return(temp)
}

# U-Chart (input: Samples, Defects/Unit)
uChart <- function(nn, d, label = "x"){
  if(length(nn) != length(d)) {
    return(cat("\n", "Input vectors have different lengths."))
  }
  temp = data.table(Sample = nn, Defects = d, U = d/nn)
  n = nrow(temp); uavg = sum(temp$Defects)/sum(temp$Sample)
  temp[, CL := uavg]
  temp[, LCL := uavg - 3*sqrt(uavg/nn)]
  temp[, UCL := uavg + 3*sqrt(uavg/nn)]
  # Plot Charts
  yupper = uavg + 1.2*(max(temp$U, temp$UCL) - uavg)
  ylower = uavg - 1.2*(uavg - min(temp$U, temp$LCL))
  plot(temp$U, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("U-Chart of", label))
  lines(seq(1,n), temp$CL, col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := U]
  temp = TestControlChart(temp)
  return(temp)
}

# C-Chart (input: Num Samples (Const), Defects)
cChart <- function(c, label = "x"){
  temp = data.table(Defects = c)
  n = length(c); cavg = mean(temp$Defects)
  temp[, CL := cavg]
  temp[, LCL := cavg - 3*sqrt(cavg)]
  temp[, UCL := cavg + 3*sqrt(cavg)]
  # Plot Charts
  yupper = cavg + 1.2*(max(temp$Defects, temp$UCL) - cavg)
  ylower = cavg - 1.2*(cavg - min(temp$Defects, temp$LCL))
  plot(temp$Defects, type = "o", pch = 20, lwd = 1.2, ylim = c(ylower,yupper), ylab = label, main = paste("C-Chart of", label))
  lines(seq(1,n), temp$CL, col = "darkgreen", lwd = 2 )
  lines(seq(1,n), temp$LCL, col = "red", lwd = 2, type = "s")
  lines(seq(1,n), temp$UCL, col = "red", lwd = 2, type = "s")  
  # Create Data for Tests
  temp[, Points := Defects]
  temp = TestControlChart(temp)
  return(temp)
}


# Test Western Electric Rules
TestControlChart <- function(temp){
  temp[, zone := ifelse((Points-CL) >= 0, (trunc(3*(Points-CL)/(UCL-CL))+1),-1*(trunc(3*(CL-Points)/(CL-LCL))+1))]
  temp$rule1 = 0; temp$rule2 = 0; temp$rule3 = 0; temp$rule4 = 0
  temp[, rule1 := ifelse((Points > UCL | Points < LCL),1,0)]
  ## Rule 2
  for(i in 1:(nrow(temp)-2) ) {
    index = which(temp[i:(i+2), zone] > 2) + (i-1)
    if(length(index) >= 2) {
      for(j in index) { temp$rule2[j] = temp$rule2[j] + 1 }
    }
    index = which(temp[i:(i+2), zone] < -2) + (i-1)
    if(length(index) >= 2) {
      for(j in index) { temp$rule2[j] = temp$rule2[j] + 1 }
    }
  }
  temp[,rule2 := ifelse((rule1 == 0 & rule2 > 0),1,0)]
  ## Rule 3
  for(i in 1:(nrow(temp)-4) ) {
    index = which(temp[i:(i+4), zone] > 1) + (i-1)
    if(length(index) >= 4) {
      for(j in index) { temp$rule3[j] = temp$rule3[j] + 1 }
    }
    index = which(temp[i:(i+4), zone] < -1) + (i-1)
    if(length(index) >= 4) {
      for(j in index) { temp$rule3[j] = temp$rule3[j] + 1 }
    }
  }
  temp[,rule3 := ifelse((rule1 == 0 & rule3 > 0),1,0)]
  ## Rule 4
  for(i in 1:(nrow(temp)-8) ) {
    index = which(temp[i:(i+8), zone] > 0) + (i-1)
    if(length(index) >= 9) temp$rule4[i+8] = 1 
    index = which(temp[i:(i+8), zone] < 0) + (i-1)
    if(length(index) >= 9) temp$rule4[i+8] = 1 
  }
  cat("\n",
      "Western Electric Rule 1: There are ",nrow(temp[rule1 > 0]), " violations.", "\n", 
      "Western Electric Rule 2: There are ",nrow(temp[rule2 > 0]), " violations.", "\n", 
      "Western Electric Rule 3: There are ",nrow(temp[rule3 > 0]), " violations.", "\n",  
      "Western Electric Rule 4: There are ",nrow(temp[rule4 > 0]), " violations.",  "\n", "\n") 
  return(temp)
}

cat("\014") 

