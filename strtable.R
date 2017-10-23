## Original source:
# str Implementation for Data Frames, June 5, 2014, By Jason Bryer
# (This article was first published on Jason.Bryer.org Blog - R, and kindly contributed to R-bloggers) 
# https://www.r-bloggers.com/str-implementation-for-data-frames/
#
# Modified by Marcus Vollmer, 19 October 2017
#
#
#' Creates a \code{data.frame} version of the str function for data.frames.
#' 
#' Note that this function only works with \code{data.frames}. The function
#' will throw an error for any other object types.
#' 
#' @param n the first n element to show
#' @param width maximum width in characters for the examples to show
#' @param n.levels the first n levels of a factor to show.
#' @param width.levels maximum width in characters for the number of levels to show.
#' @param factor.values function defining how factor examples should be printed.
#'        Possible values are \code{as.character} or \code{as.integer}.
#' @export
#' @examples
#' data(iris)
#' str(iris)
#' strtable(iris)
#' strtable(iris, factor.values=as.integer)


strtable <- function(df, n=3, width=80, n.levels=n, width.levels=width, factor.values=as.character) {
	stopifnot(is.data.frame(df))
	tab <- data.frame(variable=names(df),
					  NAs=rep(as.integer(NA), ncol(df)),
					  class=rep(as.character(NA), ncol(df)),
					  stats=rep(as.character(NA), ncol(df)),
					  levels=rep(as.character(NA), ncol(df)),
					  frequency=rep(as.character(NA), ncol(df)),
					  levelsfrequency=rep(as.character(NA), ncol(df)),					  
					  examples=rep(as.character(NA), ncol(df)),
					  stringsAsFactors=FALSE,
					  numericalstats=rep(as.character(NA), ncol(df)))

	collapse.values <- function(col, n, width) {
		result <- NA
		for(j in 1:min(n, length(col))) {
			el <- ifelse(is.numeric(col),
						 paste0(col[1:j], collapse=', '),
						 paste0('"', col[1:j], '"', collapse=', '))
			if(nchar(el) <= width) {
				result <- el
			} else {
				break
			}
		}
		if(length(col) > n) {
			return(paste0(result, ', ...'))
		} else {
			return(result)
		}
	}
	collapse.table <- function(col, n, width) {
		col1 <- levels(as.factor(col))
		col2 <- table(col)
		result <- NA
		for(j in 1:min(n, length(col1))) {
			el <- ifelse(is.numeric(col1),
						 paste0(col1[1:j], collapse=', '),
						# paste0('"', col1[1:j], '"', collapse=', ')) 
						 paste0('"', col1[1:j], '" (', col2[1:j] ,')', collapse=', '))
			if(nchar(el) <= width) {
				result <- el
			} else {
				break
			}
		}
		if(length(col1) > n) {
			return(paste0(result, ', ...'))
		} else {
			return(result)
		}
	}
	
	for(i in seq_along(df)) {
		if(is.factor(df[,i])) {
			tab[i,]$NAs <- sum(is.na(df[,i]))
			tab[i,]$class <- paste0('Factor w/ ', nlevels(df[,i]), ' levels')
			tab[i,]$levels <- collapse.values(levels(df[,i]), n=n.levels, width=width.levels)
			tab[i,]$frequency <- collapse.values(table(df[,i]), n=n.levels, width=width.levels)
			tab[i,]$levelsfrequency <- collapse.table(df[,i], n=n.levels, width=width.levels)
			tab[i,]$examples <- collapse.values(factor.values(df[,i]), n=n, width=width)
			tab[i,]$stats <- tab[i,]$levelsfrequency
		} else if (is.logical(df[,i])) {
			tab[i,]$NAs <- sum(is.na(df[,i]))
			tab[i,]$class <- class(df[,i])[1]
			tab[i,]$levels <- collapse.values(levels(as.factor(df[,i])), n=n.levels, width=width.levels)
			tab[i,]$frequency <- collapse.values(table(df[,i]), n=n.levels, width=width.levels)
			tab[i,]$levelsfrequency <- collapse.table(df[,i], n=n.levels, width=width.levels)
			tab[i,]$examples <- collapse.values(factor.values(as.factor(df[,i])), n=n, width=width)
			tab[i,]$stats <- tab[i,]$levelsfrequency
		} else if (is.numeric(df[,i])) {
		  tab[i,]$NAs <- sum(is.na(df[,i])) 
		  tab[i,]$class <- class(df[,i])[1]
		  tab[i,]$examples <- collapse.values(df[,i], n=n, width=width)
		  tab[i,]$numericalstats <- paste0('m=', sprintf("%.2f", mean(df[,i], na.rm=T)), ', sd=', sprintf("%.2f", sd(df[,i], na.rm=T)), ', q=[', paste0(sprintf("%.2f", quantile(df[,i], seq(0,1,.25), na.rm=T, type=2)), collapse=', '), ']')
		  tab[i,]$stats <- tab[i,]$numericalstats
		} else {
			tab[i,]$NAs <- sum(is.na(df[,i])) 
			tab[i,]$class <- class(df[,i])[1]
			tab[i,]$examples <- collapse.values(df[,i], n=n, width=width)
		}
		
	}
	
	class(tab) <- c('strtable', 'data.frame')
	return(tab)
}

#' Prints the results of \code{\link{strtable}}.
#' @param x result of code \code{\link{strtable}}.
#' @param ... other parameters passed to \code{\link{print.data.frame}}.
#' @export
print.strtable <- function(x, ...) {
	NextMethod(x, row.names=FALSE, ...)
}