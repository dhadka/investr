# Copyright (c) 2014 David Hadka
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

library(shiny)
library(quantmod)
library(stringr)
library(adagio)

lookupSymbols <- function(symbols, ratio=rep(1, length(symbols)), duration=25*365, from=Sys.Date()-duration, to=Sys.Date(), src="yahoo") {
	result <- list()
	
	for (i in 1:length(symbols)) {
		quotes <- getSymbols(symbols[i], from=from, to=to, warnings=FALSE, src=src, auto.assign=FALSE)
		
		if (!is.null(data)) {
			quotes <- to.monthly(quotes)
			result <- append(result, list(list(symbol=symbols[i], quotes=quotes, ratio=ratio[i])))
		}
	}
	
	merged <- do.call(merge, lapply(result, function(e) Ad(e$quotes)))
	merged <- na.locf(merged)
	colnames(merged) <- sapply(result, function(e) e$symbol)
	
	list(symbols=sapply(result, function(e) e$symbol),
			 quotes=merged)
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

to.ratio <- function(ratio, now, length) {
	if (is.function(ratio)) {
		current.ratio <- ratio(now, length)
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

invest <- function(data, ratio=data$ratio, rebalance=TRUE, initial=100000, monthly=0) {
	quotes <- data$quotes
	symbols <- data$symbols
	
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
	
	M <- length(symbols)
	N <- nrow(quotes)
	
	value <- rep(0, N)
	contribution <- rep(0, N)
	qty <- rep(0, M)
	
	current.ratio <- to.ratio(ratio, 1, N)
	qty <- qty + to.qty(initial, current.ratio, quotes[1,])
	value[1] <- to.total(qty, quotes[1,])
	contribution[1] <- initial
	next.rebalance <- seq(as.Date(time(quotes)[1]), by=paste(rebalance, "months"), length=2)[2]
	
	for (i in 2:N) {
		current.ratio <- to.ratio(ratio, i, N)
		qty <- qty + to.qty(monthly, current.ratio, quotes[i,])
		value[i] <- to.total(qty, quotes[i,])
		contribution[i] <- contribution[i-1] + monthly
		
		if (as.Date(time(quotes)[i]) == next.rebalance) {
			qty <- to.qty(value[i], current.ratio, quotes[i,])
			next.rebalance <- seq(next.rebalance, by=paste(rebalance, "months"), length=2)[2]
		}
	}
	
	list(value=xts(value, index(quotes)), contribution=xts(contribution, index(quotes)))
}

to.percent <- function(result) {
	100*(result$value - result$contribution) / result$contribution
}

to.value <- function(result) {
	result$value
}

shinyServer(function(input, output, server) {
	
	getInputs <- reactive({
		symbols <- vector()
		ratio <- vector()
		
		for (i in 1:6) {
			symbol.name <- paste("ticker.", i, sep="")
			ratio.name <- paste("percent.", i, sep="")
			
			if (!is.null(input[[symbol.name]]) && nchar(input[[symbol.name]]) > 0) {
				symbols <- append(symbols, input[[symbol.name]])
				
				if (!is.null(input[[ratio.name]]) && nchar(input[[ratio.name]]) > 0) {
					ratio <- append(ratio, as.numeric(input[[ratio.name]]))
				} else {
					ratio <- append(ratio, 0)
				}
			}
		}
		
		list(symbols=symbols, ratio=ratio)
	})
	
	getResults <- reactive({
		inputs <- getInputs()
		data <- lookupSymbols(inputs$symbols)
		
		list(
			portfolios=list(
				invest(data, ratio=inputs$ratio, rebalance=FALSE),
				invest(data, ratio=inputs$ratio, rebalance=5*12),
				invest(data, ratio=inputs$ratio, rebalance=12),
				invest(data, ratio=inputs$ratio, rebalance=6),
				invest(data, ratio=inputs$ratio, rebalance=3),
				invest(data, ratio=inputs$ratio, rebalance=1)),
			names=c(
				"Never Rebalance",
				"Rebalance Every 5 Years",
				"Rebalance Every Year",
				"Rebalance Every 6 Months",
				"Rebalance Every 3 Months",
				"Rebalance Every Month"))
	})
	
	output$plot <- renderPlot({
		results <- getResults()
		
		merged.percent <- do.call(merge, lapply(results$portfolios, to.percent))

		matplot(index(merged.percent), merged.percent, type='l', ylab="Percent Change", xlab="Date", lwd=2)
		grid(nx=NA, ny=NULL)
		legend("topleft", results$names, col=1:6, lty=1:5, lwd=2, cex=0.7)
	})
	
	output$table <- renderTable({
		results <- getResults()
		data <- matrix("", nrow=length(results$portfolios), ncol=6)

		for (i in 1:length(results$portfolios)) {
			result <- results$portfolios[[i]]
			value <- sapply(to.value(result), as.numeric)
			contribution <- sapply(result$contribution, as.numeric)
			N <- nrow(value)
			
			tot.return <- round(100*(value[N] - contribution[N]) / contribution[N], 1)
			avg.return <- 100*((tot.return/100+1)^(1/(N/12))-1)
			yearly.diff <- sapply(seq(12,N,12), function(i) (value[i]-value[i-11])/value[i-11])
			worst.year <- min(yearly.diff)*100
			best.year <- max(yearly.diff)*100
			years.loss <- 100*sum(yearly.diff < 0)/length(yearly.diff)
			
			max.sub <- maxsub(-yearly.diff)
			max.loss <- -100*max.sub$sum
			consec.yearly.loss <- max.sub$ind[2] - max.sub$ind[1] + 1
			
			data[i,1] <- sprintf("%0.1f%%", tot.return)
			data[i,2] <- sprintf("%0.1f%%", avg.return)
			data[i,3] <- sprintf("%0.1f%%", best.year)
			data[i,4] <- sprintf("%0.1f%%", worst.year)
			data[i,5] <- sprintf("%0.1f%% / %d", max.loss, consec.yearly.loss)
			data[i,6] <- sprintf("%0.0f%%", years.loss)
		}
		
		colnames(data) <- c("Total Return", "Average Return", "Best Year", "Worst Year", "Largest Loss / Years", "Years with Loss")
		data.frame("Allocation"=results$names, data, check.names=FALSE)
	}, include.rownames = FALSE, align="llrrrrrr")

})
