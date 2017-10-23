## Original source:
# characteristics_table by Marcus Vollmer, University Medicine Greifswald, Germany, 11 April 2017
# Last modified: 29 June 2017
#
# s = characteristics_table(-1, "Betablocker", ds5d, "col", prec="%.1f%%")
# require(xtable)
# out = capture.output(xtable(s, align="rp{4cm}p{3cm}rrrr"))
# out = out[6:NROW(out)-1]
# out = sub("\\{tabular\\}", "\\{longtable\\}", out)
# cat(out)

characteristics_table = function(x, y, D, rowcol, prec="%.1f%%", prec_continuous="%.0f", latex="empty"){
  j=1
  if (rowcol=="row") { rc=1 } else { rc=2}
  if (x==-1){
    x = variable.names(D)[sapply(D, class)=="factor" | sapply(D, class)=="logical"]
    x = setdiff(x,y)
    x2 = vector()
  } else {
    if (x==-2){
      x = variable.names(D)[sapply(D, class)=="factor" | sapply(D, class)=="logical"]
      x = setdiff(x,y)
      x2 = variable.names(D)[sapply(D, class)=="integer" | sapply(D, class)=="numeric"]
    }  
  }
  
  
  # Table for categorical variables
#  library(tidyr)
#  spread(data.frame(Variable=character(), Level=character()), levels(D[,y]))
#  s = cbind(, data.frame(levels(D[,y])=character()), data.frame(P=character(), NAs=integer(), stringsAsFactors=FALSE))
  s = data.frame(Variable=character(), Level=character(), No=character(), Yes=character(), P=character(), NAs=integer(), stringsAsFactors=FALSE)
  for (i in 1:length(x)) {
    jj = which(names(D)==x[i])
    if (class(D[,jj])=="logical") {
      t = table(factor(D[,jj], c(TRUE,FALSE), c("True","False")), D[,y])
    } else {
      t = table(D[,jj], D[,y])
    }
    t_pct = replace(t, TRUE, sprintf(prec, prop.table(t,rc)*100))
    
    if (prod(dim(t))>2) {
      if (prod(dim(t))>6) { t_p = chisq.test(t) } else { t_p = fisher.test(t) }
    } else { t_p$p.value=NA }
    
    for (k in 1:NROW(t)) {
      s = rbind(s, data.frame(t(c(Variable="", Level="", No="", Yes="", P="", NAs=NA)), stringsAsFactors=FALSE))
      #s = rbind(s, data.frame(t(cbind(c(Variable="", Level=""), c(No="", Yes=""), c(P="", NAs=NA))), stringsAsFactors=FALSE))
      if (k==1) {
        s$Variable[j] = x[i]
        s$P[j] = sprintf("%.4f",t_p$p.value)
        s$NAs[j] = NROW(D)-sum(t)
      }
      s$Level[j] = row.names(t)[k]
      s$No[j] = paste0(sprintf("%i",t[k,1]), " (", t_pct[k,1], ")")
      s$Yes[j] = paste0(sprintf("%i",t[k,2]), " (", t_pct[k,2], ")")
      j=j+1
    }
  }
  
  # Table for continuous variables
  if (length(x2)>0) {
    for (i in 1:length(x2)) {
      jj = which(names(D)==x2[i])
      
      s = rbind(s, data.frame(t(c(Variable="", Level="", No="", Yes="", P="", NAs=NA)), stringsAsFactors=FALSE))
      s$P[j] = sprintf("%.4f", wilcox.test(D[D[,y]==levels(D[,y])[1],jj], D[D[,y]==levels(D[,y])[2],jj])$p.value)
      
      q1 = quantile(D[D[,y]==levels(D[,y])[1],jj], c(.25, .5, .75), na.rm=TRUE)
      q2 = quantile(D[D[,y]==levels(D[,y])[2],jj], c(.25, .5, .75), na.rm=TRUE)
      
      s$No[j] = paste0(sprintf(prec_continuous,q1[2]), " (", sprintf(prec_continuous,q1[1]), ",", sprintf(prec_continuous,q1[3]), ")")
      s$Yes[j] = paste0(sprintf(prec_continuous,q2[2]), " (", sprintf(prec_continuous,q2[1]), ",", sprintf(prec_continuous,q2[3]), ")")
      
      s$Level[j] = "Median (Quartiles)"
      s$Variable[j] = x2[i]
      s$NAs[j] = sum(is.na(D[,jj]))
      j=j+1
    }
  }
  
  colnames(s)[3:4] = levels(D[,y])  

# Output as LaTeX longtable
  if (latex=="empty") {
    return(s)
  } else {
    require(xtable)
    out = capture.output(xtable(s, align=latex))
    out = out[6:NROW(out)-1]
    out = sub("\\{tabular\\}", "\\{longtable\\}", out)
    return(cat(out))
  }
  
}
