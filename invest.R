library(quantmod)
library(stringr)
options("getSymbols.warning4.0"=FALSE)

lookupSymbols <- function(symbols, conversion=to.monthly, duration=25*365, from=Sys.Date()-duration, to=Sys.Date(), src="yahoo") {
	result <- list()
	
	for (i in 1:length(symbols)) {
		quote <- getSymbols(symbols[i], from=from, to=to, warnings=FALSE, src=src, auto.assign=FALSE)
		
		if (!is.null(data)) {
			quote <- conversion(quote)
			result <- append(result, list(list(symbol=symbols[i], quote=quote)))
		}
	}
	
	merged <- do.call(merge, lapply(result, function(e) Ad(e$quote)))
	merged <- na.locf(merged)
	colnames(merged) <- sapply(result, function(e) e$symbol)
	
	merged
}

to.total <- function(qty, per.share) {
	per.share <- as.numeric(per.share)
	per.share[is.na(per.share)] <- 0
	sum(qty * per.share)
}

to.qty <- function(amount, ratio, per.share) {
	per.share <- as.numeric(per.share)
	per.share[is.na(per.share)] <- 0
	
	ratio[per.share == 0] <- 0
	
	if (sum(ratio) == 0) {
		ratio <- rep(1/length(ratio), length(ratio))
	} else {
		ratio <- ratio / sum(ratio)
	}
	
	qty <- amount * ratio / per.share
	qty[is.nan(qty)] <- 0
	qty
}

to.ratio <- function(ratio, current.time, max.time, M) {
	if (is.null(ratio)) {
		current.ratio <- rep(1/M, M)
	} else if (is.function(ratio)) {
		current.ratio <- ratio(current.time, max.time)
	} else {
		current.ratio <- ratio
	}
	
	if (sum(current.ratio) <= 0) {
		current.ratio <- rep(1/M, M)
	} else {
		current.ratio <- current.ratio / sum(current.ratio)
	}
	
	current.ratio
}

invest <- function(quotes, ratio=rep(1/ncol(quotes), ncol(quotes)), rebalance=TRUE, initial=100000, monthly=0) {
	if (is.logical(rebalance)) {
		if (rebalance) {
			rebalance <- 12
		} else {
			rebalance <- 1000000
		}
	} else if (is.character(rebalance)) {
		if (tolower(rebalance) == "yearly") {
			rebalance <- 12
		} else if (tolower(rebalance) == "monthly") {
			rebalance <- 1
		} else {
			rebalance <- 1000000
		}
	}
	
	M <- ncol(quotes)
	N <- nrow(quotes)
	value <- rep(0, N)
	contribution <- rep(0, N)
	qty <- rep(0, M)
	
	# Starting portfolio
	current.ratio <- to.ratio(ratio, 1, N, M)
	qty <- qty + to.qty(initial, current.ratio, quotes[1,])
	value[1] <- to.total(qty, quotes[1,])
	contribution[1] <- initial
	next.rebalance <- seq(as.Date(time(quotes)[1]), by=paste(rebalance, "months"), length=2)[2]
	
	# Update portfolio each month
	for (i in 2:N) {
		current.ratio <- to.ratio(ratio, i, N, M)
		qty <- qty + to.qty(monthly, current.ratio, quotes[i,])
		value[i] <- to.total(qty, quotes[i,])
		contribution[i] <- contribution[i-1] + monthly
		
		if (as.Date(time(quotes)[i]) == next.rebalance) {
			qty <- to.qty(value[i], current.ratio, quotes[i,])
			next.rebalance <- seq(next.rebalance, by=paste(rebalance, "months"), length=2)[2]
		}
	}
	
	xts(cbind(value, contribution), index(quotes))
}

to.percent <- function(result) {
	100*(result$value - result$contribution) / result$contribution
}

to.value <- function(result) {
	result$value
}

to.contribution <- function(result) {
	result$contribution
}

recessions <- data.frame(
	start=c(
		as.Date("1990-07-01"),
		as.Date("2001-03-01"),
		as.Date("2007-12-01")),
	end=c(
		as.Date("1991-03-31"),
		as.Date("2001-11-30"),
		as.Date("2009-06-30")))

plot.investment <- function(result, conversion=to.percent, show.recessions=TRUE, xlab="Date", ylab="Return", ...) {
	if (is.list(result)) {
		result <- do.call(merge, lapply(result, conversion))
	} else {
		result <- conversion(result)
	}

	if (is.null(names)) {
		names <- names(result)
	}
	
	matplot(index(result), result, type='n', axes=FALSE, xlab=NA, ylab=NA, main=NA, ...)
	
	if (show.recessions) {
		usr <- par("usr")
		
		to.frac <- function(date) {
			as.numeric(format(date, "%Y")) + (as.numeric(format(date, "%m"))-1)/12
		}

		rect(to.frac(recessions$start), usr[3], to.frac(recessions$end), usr[4], col="#DDDDDD", border=NA)
	}
	
	matplot(index(result), result, type='l', add=TRUE, ...)
	grid(nx=NA, ny=NULL)
	title(xlab=xlab, ylab=ylab)
	axis(1)
	axis(2)
	box()
}