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

lookupSymbols <- function(symbols, ratio=rep(1, length(symbols)), duration=10*365, from=Sys.Date()-duration, to=Sys.Date(), src="yahoo") {
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
		 ratio=sapply(result, function(e) e$ratio),
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

invest <- function(data, ratio=data$ratio, rebalance=TRUE, initial=100000, monthly=0) {
	quotes <- data$quotes
	symbols <- data$symbols
	
	M <- length(symbols)
	N <- nrow(quotes)
	
	value <- rep(0, N)
	contribution <- rep(0, N)
	qty <- rep(0, M)
	
	if (sum(ratio) <= 0) {
		ratio <- rep(1/M, M)
		warning("Ratio values are not postitive, defaulting to 1/M")
	} else {
		ratio <- ratio / sum(ratio)
	}
	
	qty <- qty + to.qty(initial, ratio, quotes[1,])
	value[1] <- to.total(qty, quotes[1,])
	contribution[1] <- initial
	
	for (i in 2:N) {
		qty <- qty + to.qty(monthly, ratio, quotes[i,])
		value[i] <- to.total(qty, quotes[i,])
		contribution[i] <- contribution[i-1] + monthly
		
		if (rebalance && format(time(quotes)[i], "%m") == format(time(quotes)[1], "%m")) {
			qty <- to.qty(value[i], ratio, quotes[i,])
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

to.contribution <- function(result) {
	result$contribution
}

# TODO: Since Google/Yahoo do not currently support exporting market indices,
# we have to instead rely on mutual funds to represent each asset class.

class.symbols <- c(
	"FMAGX",
	"FVDFX",
	"FDEGX",
	"FSMVX",
	"WSMGX",
	"VISVX",
	"FOSFX",
	"FSTGX",
	"FHIGX",
	"PCRIX")

class.names = c(
	"Large Growth",
	"Large Value",
	"Mid Growth",
	"Mid Value",
	"Small Growth",
	"Small Value",
	"International",
	"Gov't Bond",
	"Muni Bond",
	"Commodities")

sector.symbols <- c(
	"FSTCX",
	"FSCPX",
	"FDFAX",
	"FSENX",
	"FSAGX",
	"FSVLX",
	"FRESX",
	"FSPHX",
	"FCYIX",
	"FSRFX",
	"FSDPX",
	"FSPTX",
	"FSUTX")

sector.names <- c(
	"Communications",
	"Consumer Cyclical",
	"Consumer Defensive",
	"Energy",
	"Precious Metals",
	"Financial",
	"Real Estate",
	"Healthcare",
	"Industrial",
	"Transportation",
	"Natural Resources",
	"Technology",
	"Utilities")

years <- 10
current.year <- format(Sys.Date(), "%Y")
start <- as.Date(paste(current.year, "-01-01", sep=""))-years*365
end <- Sys.Date()
class.quotes <- lookupSymbols(class.symbols, from=start, to=end)
sector.quotes <- lookupSymbols(sector.symbols, from=start, to=end)

shinyServer(function(input, output, server) {
	
	output$ui <- renderUI({
		if (input$plot) {
			plotOutput("plot")
		} else {
			tableOutput("table")
		}
	})
	
	output$plot <- renderPlot({
		if (input$view == "Class") {
			names <- class.names
			quotes <- class.quotes	
		} else { 
			names <- sector.names
			quotes <- sector.quotes
		}
		
		data <- NULL
		
		for (j in 1:length(names)) {
			ratio <- rep(0, length(names))
			ratio[j] <- 1
			result <- invest(quotes, ratio)
			value <- as.numeric(result$value)
			contribution <- as.numeric(result$contribution)
			
			entry <- rollapply(result$value, 12, function(x) {
				x <- as.numeric(x)
				100*(x[length(x)]-x[1])/x[1]
			}, align="center")
			
			entry <- na.omit(entry)
			
			if (is.null(data)) {
				data <- entry
			} else {
				data <- merge(data, entry)
			}
		}

		matplot(index(data), data, type='l', ylab="Percent Change", xlab="Date", lwd=2)
		grid(nx=NA, ny=NULL)
		abline(h=0)
		legend("topleft", names, col=1:6, lty=1:5, cex=0.7, lwd=2)
	})
	
	output$table <- renderTable({
		if (input$view == "Class") {
			names <- class.names
			quotes <- class.quotes	
		} else { 
			names <- sector.names
			quotes <- sector.quotes
		}
	
		if (!input$cumulative) {
			data <- matrix(0, nrow=years, ncol=length(names))
			
			for (j in 1:length(names)) {
				ratio <- rep(0, length(names))
				ratio[j] <- 1
				result <- invest(quotes, ratio)
				value <- as.numeric(result$value)
				contribution <- as.numeric(result$contribution)
				data[,j] <- sapply(seq(12,length(value),12), function(i) 100*(value[i]-value[i-11])/value[i-11])
			}
			
			colnames(data) <- names
			rownames(data) <- sprintf("%d", (2013-years+1):2013)
			
			output <- matrix("", nrow=length(names), ncol=years)
			
			for (i in 1:years) {
				ordering <- rev(order(data[i,]))
				
				for (j in 1:length(names)) {
					output[j,i] <- paste("<div style=\"text-align: center\">", names[ordering[j]], "<br><font size=\"-2\">", round(data[i,ordering[j]], 1), "%</font></div>", sep="")
				}
			}
			
			colnames(output) <- sprintf("%d", (2013-years+1):2013)
		} else {
			periods <- 6
			period.names <- c("Last Month", "Last 3 Months", "Last 6 Months", "Last Year", "Last 5 Years", "Last 10 Years")
			data <- matrix(0, nrow=periods, ncol=length(names))
			
			for (j in 1:length(names)) {
				ratio <- rep(0, length(names))
				ratio[j] <- 1
				result <- invest(quotes, ratio)
				value <- as.numeric(result$value)
				contribution <- as.numeric(result$contribution)
				N <- length(value)
				
				data[1,j] <- 100*(value[N]-value[N-1])/value[N-1]
				data[2,j] <- 100*(value[N]-value[N-3])/value[N-3]
				data[3,j] <- 100*(value[N]-value[N-6])/value[N-6]
				data[4,j] <- 100*(value[N]-value[N-12])/value[N-12]
				data[5,j] <- 100*(value[N]-value[N-5*12])/value[N-5*12]
				data[6,j] <- 100*(value[N]-value[1])/value[1]
			}
			
			colnames(data) <- names
			rownames(data) <- period.names
			
			output <- matrix("", nrow=length(names), ncol=periods)
			
			for (i in 1:periods) {
				ordering <- rev(order(data[i,]))
				
				for (j in 1:length(names)) {
					output[j,i] <- paste("<div style=\"text-align: center;\">", names[ordering[j]], "<br><font size=\"-2\">", round(data[i,ordering[j]], 1), "%</font></div>", sep="")
				}
			}
			
			colnames(output) <- period.names
		}
		
		output
	},
	include.rownames = FALSE,
	sanitize.text.function = function(x) x,
	sanitize.colnames.function = function(x) paste("<div style=\"text-align: center\">", x, "</div>", sep=""))
	
})
