## Original source:
# Stargazer tables by Marek Hlavac, Harvard University. E-mail: hlavac at fas.harvard.edu
# Stargazer_long modification by Marcus Vollmer, University Medicine Greifswald, Germany, 11 April 2017

library(stargazer)
library(stringr)

stargazer_long = function(x, summary=FALSE, rownames=FALSE, single.row=FALSE, output="", rotate=0, column.sep.width="5pt", digits=NULL, ci=TRUE){
  out = capture.output(stargazer(x, summary=summary, rownames=rownames, type="latex", column.sep.width=column.sep.width, single.row=single.row, digits=digits, ci=ci))
  out = sub("\\\\begin\\{table\\}", "", out)
  out = sub("\\[!htbp\\] \\\\centering", "", out)
  out = sub("\\\\caption\\{\\}", "", out)
  out = sub("\\\\label\\{\\}", "", out)
  out = sub("\\\\end\\{table\\}", "", out)
  out = sub("\\{tabular\\}", "\\{longtable\\}", out)
  if (output!="") { out[7] = sub(paste0(rep("c", str_count(output,"c")+str_count(output,"p")-str_count(output,"m")), collapse=""), output, out[7]) }
  out = out[7:NROW(out)]
  if (rotate!=0) {
    out[4] = gsub("&", paste0("} & \\\\rotatebox\\{", rotate, "\\}\\{"), out[4])
    out[4] = paste0("{", sub("\\\\\\\\", "\\}\\\\\\\\", out[4]))
  }
  return(cat(out))
}
