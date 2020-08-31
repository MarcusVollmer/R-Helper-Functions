## Original source:
# characteristics_table by Marcus Vollmer, University Medicine Greifswald, Germany, 11 April 2017
# Last modified: 31 August 2020
#
#

characteristics_table = function(x, y, D, rowcol, prec="%.1f", prec_continuous="%.0f", prec_p="%.4f", latex="empty", tablefootnote=TRUE, fn=c("\u00B9","\u00B2","\u00B3","\u2074","\u2075","\u2076"), fn_warn="*"){
  tests = rep(0,length(fn))
  tests_label = c("Chi-squared test", "Fisher's exact test", "Kruskal-Wallis rank sum test", "Wilcoxon rank sum test", "One-way analysis of variance (ANOVA)", "Welch's t-test")
  
  # Handle warnings
  wa = FALSE
  oldw <- getOption("warn")
  options(warn=0)
  #assign("last.warning", NULL, envir = baseenv())
  
# Identify feature classes
  if (rowcol=="row") { rc=1 } else { rc=2 }
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
  
  if (class(D[,y])=="logical"){
    D[,y] = as.factor(D[,y])
  }
  
# Initialize data frame
  s1 = data.frame(Variable=character(), Level=character(), P=character(), NAs=integer(), stringsAsFactors=FALSE)
  s2 = model.matrix(as.formula(paste(x[1], "~", y, "-1")), D[0,]) 
  colnames(s2) = levels(D[,y])
  s = cbind(s1, s2)
  new.row = s[1,][NA,]
  new.row$Variable = ""
  new.row$P = ""
  new.row$NAs = ""
  
# Table for categorical variables
  j=1
  for (i in 1:length(x)) {
    jj = which(names(D)==x[i])
    if (class(D[,jj])=="logical") {
      t = table(factor(D[,jj], c(TRUE,FALSE), c("True","False")), D[,y])
    } else {
      t = table(D[,jj], D[,y])
    }
    t_pct = replace(t, TRUE, sprintf(prec, prop.table(t,rc)*100))
    
    if (prod(dim(t))>2) {
      if (prod(dim(t))>6) { 
        t_p = chisq.test(t)
        test = 1
      } else {
          t_p = fisher.test(t)
          test = 2
      }
    } else { 
      t_p$p.value=NA
      test = NA
    }

    
    for (k in 1:NROW(t)) {
      s = rbind(s, new.row)
      if (k==1) {
        s$Variable[j] = x[i]
        s$NAs[j] = NROW(D)-sum(t)
        s$P[j] = sprintf(prec_p,t_p$p.value)
        
        if (tablefootnote==TRUE & !is.na(test)) {
          s$P[j] = paste0(s$P[j], fn[test])
          tests[test] = 1
        }
        
        # Handling of warnings
        if (!is.na(test)) {
            if (test==1) {
              tryCatch( t_p <- chisq.test(t),
                warning = function(w) {
                  wa <<- TRUE
                  s$P[j] <<- paste0(s$P[j], fn_warn)
                }
              )
            }
        }
      }
      s$Level[j] = row.names(t)[k]
      for (l in 1:NROW(levels(D[,y]))) {
        s[j,levels(D[,y])[l]] = paste0(sprintf("%i",t[k,l]), " (", t_pct[k,l], ")")
      }
      j=j+1
    }
  }
  
# Table for continuous variables
  if (length(x2)>0) {
    for (i in 1:length(x2)) {
      jj = which(names(D)==x2[i])
      
      # Median with quartiles and parameter-free test (rankbased)
      s = rbind(s, new.row)
      if (NROW(levels(D[,y]))>1) {
        if (NROW(levels(D[,y]))>2) {
          s$P[j] = sprintf(prec_p, kruskal.test(D[,jj], D[,y])$p.value)
          test = 3
        } else { 
          s$P[j] = sprintf(prec_p, wilcox.test(D[D[,y]==levels(D[,y])[1],jj], D[D[,y]==levels(D[,y])[2],jj])$p.value)
          test = 4
        }
      } else {
        s$P[j]=NA
        test = NA
      }
      
      
      if (tablefootnote==TRUE & !is.na(test)) {
        s$P[j] = paste0(s$P[j], fn[test])
        tests[test] = 1
      }
      
      for (l in 1:NROW(levels(D[,y]))) {
        q = quantile(D[D[,y]==levels(D[,y])[l],jj], c(.25, .5, .75), na.rm=TRUE)
        s[j,levels(D[,y])[l]] = paste0(sprintf(prec_continuous,q[2]), " (", sprintf(prec_continuous,q[1]), ",", sprintf(prec_continuous,q[3]), ")")
      }

      s$Level[j] = "Median (Quartiles)"
      s$Variable[j] = x2[i]
      s$NAs[j] = sum(is.na(D[,jj]))
      j=j+1
      
      # Mean with standard deviation and parameter test
      s = rbind(s, new.row)
      if (NROW(levels(D[,y]))>1) {
        if (NROW(levels(D[,y]))>2) {
          s$P[j] = sprintf(prec_p, summary(aov(as.formula(paste(x2[i], "~", y)), data=D))[[1]][["Pr(>F)"]][1])
          test = 5
        } else { 
          s$P[j] = sprintf(prec_p, t.test(D[,jj]~D[,y])$p.value)
          test = 6
        }
      } else { 
        s$P[j]=NA 
        test = NA
      }
      
      if (tablefootnote==TRUE & !is.na(test)) {
        s$P[j] = paste0(s$P[j], fn[test])
        tests[test] = 1
      }

      for (l in 1:NROW(levels(D[,y]))) {
        mean1 = mean(D[D[,y]==levels(D[,y])[l],jj], na.rm=TRUE)
        sd1 = sd(D[D[,y]==levels(D[,y])[l],jj], na.rm=TRUE)
        s[j,levels(D[,y])[l]] = paste0(sprintf(prec_continuous,mean1), " (", sprintf(prec_continuous,sd1), ")")
      }

      s$Level[j] = "Mean (SD)"
      s$Variable[j] = x2[i]
      s$NAs[j] = sum(is.na(D[,jj]))
      j=j+1      
    }
  }
  s = s[,c(1:2,5:NCOL(s),3:4)]
  
# Restore warnings option
  options(warn = oldw)
  
# Output as LaTeX longtable
  if (latex=="empty") {
    return(s)
  } else {
    library(devtools)
    repo = "https://raw.githubusercontent.com/MarcusVollmer/R-Helper-Functions/master/"
    source_url(paste0(repo,"stargazer_long.R"))
    out = capture.output(stargazer_long(s, summary=FALSE, rownames=FALSE, output=latex))
    if (tablefootnote==TRUE) {
      if (wa==TRUE) {
        fn1 = paste0("\\\\multicolumn\\{", NCOL(s), "\\}\\{l\\}\\{", "$\\ ^\\{\\\\textasteriskcentered\\}$ Chi-squared approximation may be incorrect", "\\}\\\\\\\\ ")
      } else {fn1=""}
      
      fn2 = ""
      for (i in 1:length(tests)) {
        if (tests[i]==1) {
          fn2 = paste(fn2, paste0("\\\\multicolumn\\{", NCOL(s), "\\}\\{l\\}\\{", fn[i], " ", tests_label[i], "\\}\\\\\\\\ "))
        }
      }
      
      out = sub("\\\\end\\{longtable", paste0(fn1, fn2, "\\\\end\\{longtable"), out)
    }
    return(cat(out))
  }
  
}
