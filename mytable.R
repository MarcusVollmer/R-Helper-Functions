## Original source:
# mytable by Marcus Vollmer, University Medicine Greifswald, Germany, 11 April 2017

mytable = function(x,y,z){
  t = table(x, y)
  t_pct = replace(t, TRUE, sprintf(z, prop.table(t,1)*100))
  print(rbind(t,t_pct))
  
  if (prod(dim(t))>6) {
    t_p = chisq.test(t)
    cat("Pearson's Chi-squared test: p-Value=", sprintf("%.2e",t_p$p.value), "\n")    
  } else {
    t_p = fisher.test(t)
    cat("Fisher's Exact Test for Count Data: p-Value=", sprintf("%.2e",t_p$p.value), "\n")
  }
}