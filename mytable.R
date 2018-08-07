## Original source:
# mytable by Marcus Vollmer, University Medicine Greifswald, Germany, 11 April 2017
# Last modified: 07 August 2018

mytable = function(x, y, ci=FALSE, prec="%.1f", prec_p="%.2e", latex="empty", pct_sign=FALSE){
  if (pct_sign==FALSE) {
    pct_sign=''
  }
  
  t = table(x, y)
  t_pct = replace(t, TRUE, sprintf(prec, prop.table(t,1)*100))
  
  # Compute Clopper-Pearson confidence intervals
  if (is.numeric(ci)) {
    require(PropCIs)
    if (class(y)=="logical") {
      t_ci = table(CI=character())
      for (k in 1:NROW(t)) {
        c = sprintf(prec, 100*exactci(t[k,"TRUE"], sum(t[k,]), ci)$conf.int[1:2])
        t_ci = rbind(t_ci, paste(c[1], "to", c[2]))
      } 
    } else { if (class(y)=="factor") {
        t_ci = matrix(as.character(1:(NROW(t)*NCOL(t))), nrow=NROW(t))
        for (k in 1:NROW(t)) {
          for (l in 1:NCOL(t)) {
            c = sprintf(prec, 100*exactci(t[k,l], sum(t[k,]), ci)$conf.int[1:2])
            t_ci[k,l] = paste0(c[1], pct_sign, " to ", c[2], pct_sign)
          }
        }       
      }
    }
  }
  
  # Perform homogeneity test
  if (prod(dim(t))>6) {
    t_p = chisq.test(t)
    testresult = paste0("Pearson's Chi-squared test: p-Value=", sprintf(prec_p,t_p$p.value))    
  } else {
    t_p = fisher.test(t)
    testresult = paste0("Fisher's Exact Test for Count Data: p-Value=", sprintf(prec_p,t_p$p.value))
  }
  
  # Cat results
  if (is.numeric(ci)) { 
    if (class(y)=="logical") {
      for (k in 1:NCOL(t)) {
        t[,k] = paste0(t[,k], " (", t_pct[,k], pct_sign, ")")
      }
      t = cbind(t,t_ci)
      colnames(t)[NCOL(t)] = paste(sprintf("%.0f",100*ci), "CI")
    } else { if (class(y)=="factor") {
      for (k in 1:NROW(t)) {
        for (l in 1:NCOL(t)) {
          t[k,l] = paste0(t[k,l], " (", t_pct[k,l], pct_sign, ", ", t_ci[k,l], ")")
          }
        }
      }
    }
  } else {
    for (k in 1:NCOL(t)) {
      t[,k] = paste0(t[,k], " (", t_pct[,k], pct_sign, ")")
    }
  }
  
  # Output as LaTeX longtable
  if (latex=="empty") {
    print(t)
    print(testresult)
    return(t)
  } else {
    if (latex==TRUE) {
      latex = strrep('r',NCOL(t)+1)
    }
    out = capture.output(stargazer_long(as.data.frame.matrix(t), summary=FALSE, rownames=TRUE, output=latex))
    out = sub("end\\{longtable", paste0("multicolumn\\{", NCOL(t)+1, "\\}\\{l\\}\\{", testresult, "\\}\\\\end\\{longtable"), out)
    return(cat(out))
  }
}