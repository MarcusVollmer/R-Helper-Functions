## Original source:
# mytable by Marcus Vollmer, University Medicine Greifswald, Germany, 11 April 2017
# Last modified: 08 August 2018

mytable = function(x, y, z=NULL, ci=FALSE, prec="%.1f", prec_p="%.2e", latex="empty", pct_sign=FALSE, tablefootnote=TRUE, fn_warn="*"){
  tests_label = c("Pearson's Chi-squared test", "Fisher's exact test", "Kruskal-Wallis rank sum test", "Wilcoxon rank sum test", "One-way analysis of variance (ANOVA)", "Student's t-test")
  tests_format = c("", "", "med ($q_\\{25\\}$,$q_\\{75\\}$)", "med ($q_\\{25\\}$,$q_\\{75\\}$)", "mean (sd)", "mean (sd)")
  test = NA
  
  # Handle warnings
  wa = FALSE
  oldw <- getOption("warn")
  options(warn=0)
  #assign("last.warning", NULL, envir = baseenv())

  
  if (pct_sign==FALSE) {
    pct_sign=''
  }

  if (is.null(z) | !is.numeric(z)) {
    # Crosstabs with counts 
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
        # Handling of warnings
        tryCatch( 
          {
            t_p <- chisq.test(t)
            testresult <- paste0(tests_label[1], ": p-Value=", sprintf(prec_p,t_p$p.value))  
          }, warning = function(w) {
            wa <<- TRUE
            warn_message <<- "Chi-squared approximation may be incorrect"
            testresult <<- paste0(tests_label[1], ": p-Value=", sprintf(prec_p,t_p$p.value), fn_warn)
          }
        )
        
      } else {
        t_p = fisher.test(t)
        testresult = paste0(tests_label[2], ": p-Value=", sprintf(prec_p,t_p$p.value))
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
      
  } else {
    # Crosstabs with means of variable z
      t = table(x, y)
      t1 = t
      t2 = t
      p = t
      rn = rownames(t)
      cn = colnames(t)
      
      D = data.frame(z=z,g=interaction(x,y))
      g = levels(D$g)
      
      # Compute means, sd, median, quartiles and Shapiro-Wilk test of normality
      for (i in 1:length(g)) {
        tmp = D$z[D$g==g[i]]
        
        t1[i] = paste0(sprintf(prec,mean(tmp, na.rm=TRUE)), " (", sprintf(prec,sd(tmp, na.rm=TRUE)), ")")
        
        q = quantile(tmp, c(.25, .5, .75), na.rm=TRUE)
        t2[i] = paste0(sprintf(prec,q[2]), " (", sprintf(prec,q[1]), ",", sprintf(prec,q[3]), ")")
        
        p[i] = shapiro.test(tmp)$p.value
      }
      
      # row-wise testing
      p_row = NA
      for (j in 1:NROW(t)) {
        if (sum(p>=.05)==length(g)) {
          # parametric tests
          if (NCOL(t)>2) {
            p_row[j] = sprintf(prec_p, summary(aov(as.formula("z~g"), data=D[x==rn[j],]))[[1]][["Pr(>F)"]][1])
            test = 5
          } else { 
            p_row[j] = sprintf(prec_p, t.test(as.formula("z~g"), data=D[x==rn[j],])$p.value)
            test = 6
          }
        } else {
          # non-parametric tests
          if (NCOL(t)>2) {
            test = 3
            p_row[j] = sprintf(prec_p, kruskal.test(as.formula("z~g"), data=D[x==rn[j],])$p.value)
          } else { 
            # Handling of warnings
            tryCatch(
              {
                test <- 4
                t_p <- wilcox.test(as.formula("z~g"), data=D[x==rn[j],])
                p_row[j] <- sprintf(prec_p, t_p$p.value)
              }, warning = function(w) {
                wa <<- TRUE
                warn_message <<- "Cannot compute exact p-value with ties"
                p_row[j] <<- paste0(sprintf(prec_p, t_p$p.value), fn_warn)
              }
            )
          }          
        }
      }
      
      if (sum(p>=.05)==length(g)) {
        t = cbind(t1, p=p_row)
        testresult = paste0(tests_label[5], ": p-Value=", sprintf(prec_p, summary(aov(as.formula("z~g"), data=D))[[1]][["Pr(>F)"]][1]))
      } else {
        t = cbind(t2, p=p_row)
        testresult = paste0(tests_label[3], ": p-Value=", sprintf(prec_p, kruskal.test(D$z, D$g)$p.value))
      }

  }
  
  
# Output as LaTeX longtable
  if (latex=="empty") {
    print(t)
    print(testresult)
    return(t)
  } else {
    if (latex==TRUE) {
      latex = strrep('r', NCOL(t)+1)
    }
    source("stargazer_long.R")
    out = capture.output(stargazer_long(as.data.frame.matrix(t), summary=FALSE, rownames=TRUE, output=latex))
    if (tablefootnote==TRUE) {
      if (wa==TRUE) {
        fn1 = paste0("\\\\multicolumn\\{", NCOL(t)+1, "\\}\\{l\\}\\{", "$\\ ^\\{\\\\textasteriskcentered\\}$ ", warn_message, "\\}\\\\\\\\ ")
      } else {fn1=""}
      
      fn2 = ""
      if (!is.na(test)) {
        fn2 = paste0("\\\\multicolumn\\{", NCOL(t)+1, "\\}\\{l\\}\\{Line by line: ", tests_format[test], ", ", tests_label[test], "\\}\\\\\\\\ ")
      }
      
      fn_overall = paste0("\\\\multicolumn\\{", NCOL(t)+1, "\\}\\{l\\}\\{Overall: ", testresult, "\\}\\\\\\\\ ") 
      
      out = sub("\\\\end\\{longtable", paste0(fn2, fn_overall, fn1, "\\\\end\\{longtable"), out)
    }
    return(cat(out))  
  }
}