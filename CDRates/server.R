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
library(rjson)
library(XML)
library(RCurl)
library(quantmod)
library(stringr)

createOption <- function(provider, name, term, rate, apy, minimum=0, maximum=1000000000, min.term=0) {
	list(provider=provider, name=name, term=term, rate=rate, apy=apy, minimum=minimum, maximum=maximum, min.term=min.term)
}

to.rate <- function(apy, periods) {
	((apy+1)^(1/periods) - 1)*periods
}

options <- list()
ssl.verifypeer <- FALSE

#GE Capital Bank
tryCatch({
	result <- getURL("https://rsge-migrate.fuseservice.com/Rates.asmx/GetDefaultRatesForInstrument?format=json&instrument=%22Savings%20Product%22&accessCode=%22616D6A00-D2BB-48AA-8F09-D8E39FEB245F%22&affiliate=%22GCB%22", ssl.verifypeer=ssl.verifypeer, .opts=list(sslversion=3))
	json <- fromJSON(substring(result, 2, nchar(result)-2))
	
	for (entry in json$d$Rates) {
		options <- append(options, list(createOption(provider="GE Capital", name="GE Capital Savings", term=0, rate=entry$AffiliateAPR, apy=entry$AffiliateAPY, minimum=entry$TierMin, maximum=entry$TierMax)))					 
	}
	
	result <- getURL("https://rsge-migrate.fuseservice.com/Rates.asmx/GetDefaultRatesForInstrument?format=json&instrument=%22CD%22&accessCode=%22616D6A00-D2BB-48AA-8F09-D8E39FEB245F%22&affiliate=%22GCB%22", ssl.verifypeer=ssl.verifypeer, .opts=list(sslversion=3))
	json <- fromJSON(substring(result, 2, nchar(result)-2))
	
	for (entry in json$d$Rates) {
		options <- append(options, list(createOption(provider="GE Capital", name=sprintf("GE Capital %d Month CD", entry$TermLength), term=entry$TermLength, rate=entry$AffiliateAPR, apy=entry$AffiliateAPY, minimum=entry$TierMin, maximum=entry$TierMax)))					 
	}
}, error=function(c) { print(c) })

#Barclays
tryCatch({
	result <- getURL("https://www.banking.barclaysus.com/svlt/rates.json", ssl.verifypeer=ssl.verifypeer)
	json <- fromJSON(result)
	
	for (product in json$products) {
		if (product$type == "3500") {
			options <- append(options, list(createOption(provider="Barclays", name=sprintf("Barclays %d Month CD", as.numeric(product$term)), term=as.numeric(product$term), rate=as.numeric(product$rate), apy=as.numeric(product$apy))))
		} else if (product$type == "3000") {
			options <- append(options, list(createOption(provider="Barclays", name="Barclays Savings", term=0, rate=as.numeric(product$rate), apy=as.numeric(product$apy))))
		}
	}
}, error=function(c) { print(c) })

#Ally
tryCatch({
	result <- getURL("http://www.ally.com/rss/rates.xml")
	doc <- xmlParse(result)
	items <- getNodeSet(doc, "//channel/item")
	
	for (item in items) {
		title <- xmlElementsByTagName(item, "title")[[1]]
		
		if (xmlValue(title) == "High Yield") {
			description <- xmlElementsByTagName(item, "description")[[1]]
			table <- readHTMLTable(xmlValue(description))[[1]]
	
			for (i in 1:nrow(table)) {
				term <- unlist(strsplit(as.character(table[i,1]), "\\s"))
				term <- as.numeric(term[1]) * ifelse(term[2] == "month", 1, 12)
				rate <- as.character(table[i,2])
				rate <- as.numeric(substring(rate, 0, nchar(rate)-1)) / 100
				apy <- as.character(table[i,3])
				apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
				
				options <- append(options, list(createOption(provider="Ally", name=sprintf("Ally %d Month CD", term), term=term, rate=rate, apy=apy)))
			}
		} else if (xmlValue(title) == "Raise Your Rate") {
			description <- xmlElementsByTagName(item, "description")[[1]]
			table <- readHTMLTable(xmlValue(description))[[1]]
			
			for (i in 1:nrow(table)) {
				term <- unlist(strsplit(as.character(table[i,1]), "\\s"))
				term <- as.numeric(term[1]) * ifelse(term[2] == "month", 1, 12)
				rate <- as.character(table[i,2])
				rate <- as.numeric(substring(rate, 0, nchar(rate)-1)) / 100
				apy <- as.character(table[i,3])
				apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
				
				options <- append(options, list(createOption(provider="Ally", name=sprintf("Ally %d Month Raise Your Rate CD", term), term=term, rate=rate, apy=apy)))
			}
		} else if (xmlValue(title) == "Online Savings") {
			description <- xmlElementsByTagName(item, "description")[[1]]
			table <- readHTMLTable(xmlValue(description))[[1]]
			
			for (i in 1:nrow(table)) {
				rate <- as.character(table[i,1])
				rate <- as.numeric(substring(rate, 0, nchar(rate)-1)) / 100
				apy <- as.character(table[i,2])
				apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
				
				options <- append(options, list(createOption(provider="Ally", name="Ally Savings", term=0, rate=rate, apy=apy)))
			}
		}
	}
}, error=function(c) { print(c) })

#American Express
tryCatch({
	result <- getURL("https://personalsavings.americanexpress.com/rates.json", ssl.verifypeer=ssl.verifypeer)
	json <- fromJSON(result)
	
	for (product in json$products) {
		if (as.numeric(product$type) > 3500 && as.numeric(product$type) < 3600) {
			product$term <- sub("M$", "", str_trim(product$term)) # Clean entry ending with "M\r"
			options <- append(options, list(createOption(provider="American Express", name=sprintf("American Express %d Month CD", as.numeric(product$term)), term=as.numeric(product$term), rate=as.numeric(product$rate)/100, apy=as.numeric(product$apy)/100)))
		} else if (product$type == "3200") {
			options <- append(options, list(createOption(provider="American Express", name="American Express Savings", term=0, rate=as.numeric(product$rate)/100, apy=as.numeric(product$apy)/100)))
		}
	}
}, error=function(c) { print(c) })

#EverBank
tryCatch({
	result <- getURL("https://www.everbank.com/banking/cd", ssl.verifypeer=ssl.verifypeer)
	table <- readHTMLTable(result)[[1]]
	
	for (i in 1:nrow(table)) {
		term <- unlist(strsplit(as.character(table[i,1]), "\\s"))
		term <- as.numeric(term[1]) * ifelse(term[2] == "month", 1, 12)
		rate <- as.character(table[i,2])
		rate <- as.numeric(substring(rate, 0, nchar(rate)-1)) / 100
		apy <- as.character(table[i,3])
		apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
		
		options <- append(options, list(createOption(provider="EverBank", name=sprintf("EverBank %d Month CD", term), term=term, rate=rate, apy=apy, minimum=1500)))
	}
}, error=function(c) { print(c) })

#Synchrony Optimizer Plus
tryCatch({
	result <- getURL("https://myoptimizerplus.com/banking/products/cd/index.htm", ssl.verifypeer=ssl.verifypeer)
	table <- readHTMLTable(result)[[1]]
	
	for (j in 2:ncol(table)) {
		amount <- unlist(strsplit(colnames(table)[j], "-", fixed=TRUE))
		
		if (length(amount) == 1) {
			minimum <- as.numeric(gsub("[$,+]", "", amount[1]))
			maximum <- 1000000
		} else {
			minimum <- as.numeric(gsub("[$,+]", "", amount[1]))
			maximum <- as.numeric(gsub("[$,+]", "", amount[2]))
		}
		
		for (i in 2:nrow(table)) {
			term <- unlist(strsplit(as.character(table[i,1]), "\\s"))
			term <- as.numeric(term[1]) * ifelse(tolower(term[2]) == "month", 1, 12)
			apy <- as.character(table[i,j])
			apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
			
			options <- append(options, list(createOption(provider="Synchrony", name=sprintf("Synchrony %d Month CD", term), term=term, rate=to.rate(apy, 365), apy=apy, minimum=minimum, maximum=maximum)))
		}
	}

	result <- getURL("https://myoptimizerplus.com/banking/products/high-yield-saving/index.htm", ssl.verifypeer=ssl.verifypeer)
	table <- readHTMLTable(result)[[1]]
	
	for (i in 2:nrow(table)) {
		amount <- unlist(strsplit(as.character(table[i,1]), "-", fixed=TRUE))
		
		if (length(amount) == 1) {
			if (substring(amount[1], 0, 6) == "Up to ") {
				minimum <- 0
				maximum <- as.numeric(gsub("[$,+]", "", substring(amount[1], 6, nchar(amount[1]))))
			} else {
				minimum <- as.numeric(gsub("[$,+]", "", amount[1]))
				maximum <- 1000000
			}
		} else {
			minimum <- as.numeric(gsub("[$,+]", "", amount[1]))
			maximum <- as.numeric(gsub("[$,+]", "", amount[2]))
		}
		
		apy <- as.character(table[i,2])
		apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
		
		options <- append(options, list(createOption(provider="Synchrony", name=sprintf("Synchrony Savings", term), term=0, rate=to.rate(apy, 365), apy=apy, minimum=minimum, maximum=maximum)))
	}
}, error=function(c) { print(c) })

#TreasuryDirect
tryCatch({
	result <- getURL("https://www.treasurydirect.gov/indiv/research/indepth/ibonds/res_ibonds_iratesandterms.htm", ssl.verifypeer=ssl.verifypeer)
	tables <- readHTMLTable(result)
	
	fixed.rate <- as.character(tables[[3]][1,2])
	fixed.rate <- as.numeric(substring(fixed.rate, 0, nchar(fixed.rate)-1)) / 100
	inflation.rate <- as.character(tables[[4]][1,2])
	inflation.rate <- as.numeric(substring(inflation.rate, 0, nchar(inflation.rate)-1)) / 100
	composite.rate <- fixed.rate + 2*inflation.rate + fixed.rate*inflation.rate
	options <- append(options, list(createOption(provider="TreasuryDirect", name="US I Savings Bond", term=60, rate=composite.rate, apy=composite.rate, min.term=12, minimum=25)))
	
	result <- getURL("https://www.treasurydirect.gov/TA_WS/securities/auctioned?pagesize=20&type=Note&format=jsonp&callback=?")
	json <- fromJSON(substring(result, 4, nchar(result)-2))
	seen.terms <- vector()
	
	for (entry in json) {
		term <- entry$term
		yield <- as.numeric(entry$highYield) / 100
		minimum <- as.numeric(entry$minimumToIssue)
		term.months <- unlist(strsplit(term, "-", fixed=TRUE))[1]
		term.months <- as.numeric(term.months)*12
		
		if (!(term %in% seen.terms)) {
			seen.terms <- append(seen.terms, term)
			options <- append(options, list(createOption(provider="TreasuryDirect", name=sprintf("%s US Treasury Note", term), term=term.months, rate=yield, apy=yield, min.term=term.months, minimum=minimum)))
		}
	}
}, error=function(c) { print(c) })

# Convert to data frame
options <- data.frame(
	provider=sapply(options, function(o) o$provider),
	name=sapply(options, function(o) o$name),
	term=sapply(options, function(o) o$term),
	rate=sapply(options, function(o) o$rate),
	apy=sapply(options, function(o) o$apy),
	minimum=sapply(options, function(o) o$minimum),
	maximum=sapply(options, function(o) o$maximum),
	min.term=sapply(options, function(o) o$min.term),
	stringsAsFactors=FALSE)

penalties=list(
	"Barclays"=function(n, ...) ifelse(n <= 24, 90, 180),
	"Ally"=function(n, ...) ifelse(n <= 24, 60, 30*n/12),
	"American Express"=function(n, ...) ifelse(n <= 12, 90, 180),
	"EverBank"=function(n, ...) 365/12 * n/4,
	"TreasuryDirect"=function(n, o) ifelse(o$name=="US I Savings Bond", 90, 365*n/12),
	"Synchrony"=function(n, ...) ifelse(n <= 12, 90, 180),
	"GE Capital"=function(n, ...) ifelse(n <= 12, 90, ifelse(n <= 60, 270, 365)))

taxes=list(
	"Barclays"=c(TRUE, TRUE, TRUE),
	"Ally"=c(TRUE, TRUE, TRUE),
	"American Express"=c(TRUE, TRUE, TRUE),
	"EverBank"=c(TRUE, TRUE, TRUE),
	"TreasuryDirect"=c(TRUE, FALSE, FALSE),
	"Synchrony"=c(TRUE, TRUE, TRUE),
	"GE Capital"=c(TRUE, TRUE, TRUE))

calc.earnings <- function(option, held, principal) {
	principal*(1+round(option$rate,4)/365)^(held*365) - calc.penalty(option, held, principal) - principal
}

calc.penalty <- function(option, held, principal) {
	ifelse((option$term > 0) && ((12*held) %% option$term > 0), principal*(1+round(option$rate,4)/365)^penalties[[option$provider]](option$term, option) - principal, 0)
}

calc.estapy <- function(option, held, principal) {
	calc.earnings(option, held, principal)/principal/held
}

calc.actearnings <- function(option, held, principal, tax.rate) {
	calc.earnings(option, held, principal)*(1-sum(tax.rate * taxes[[option$provider]]))
}

calc.actapy <- function(option, held, principal, tax.rate) {
	calc.actearnings(option, held, principal, tax.rate)/principal/held
}

# Compute inflation rate for last 6 months
CPIAUCNS <- getSymbols("CPIAUCNS", src="FRED", auto.assign=FALSE, warnings=FALSE)
last.cpi <- as.numeric(CPIAUCNS[nrow(CPIAUCNS),1])
previous.cpi <- as.numeric(CPIAUCNS[nrow(CPIAUCNS)-12,])
inflation <- (last.cpi - previous.cpi) / (previous.cpi)

shinyServer(function(input, output, session) {
	
	output$table <- renderTable({
		principal <- input$principal
		held <- input$held
		
		if (input$taxable) {
			tax.rate <- c(input$federal/100, input$state/100, input$local/100)
		} else {
			tax.rate <- c(0, 0, 0)
		}
		
		available.options <- options[principal >= options$minimum & principal <= options$maximum & held*12 >= options$min.term,]
		
		earnings <- sapply(1:nrow(available.options), function(i) calc.earnings(available.options[i,], held, principal))
		penalty <- sapply(1:nrow(available.options), function(i) calc.penalty(available.options[i,], held, principal))
		estapy <- sapply(1:nrow(available.options), function(i) calc.estapy(available.options[i,], held, principal))
		actearnings <- sapply(1:nrow(available.options), function(i) calc.actearnings(available.options[i,], held, principal, tax.rate))
		actapy <- sapply(1:nrow(available.options), function(i) calc.actapy(available.options[i,], held, principal, tax.rate))
		
		result <- data.frame(available.options, earnings, penalty, estapy, actearnings, actapy)
		ranking <- rev(order(result[,ncol(result)]))
		
		display <- matrix("", nrow=nrow(result), ncol=5)
		display[,1] <- ifelse(result$provider == "TreasuryDirect", sprintf("%s<sup>3</sup>", result$name), result$name)
		display[,2] <- sprintf("%0.2f%%", 100*result$apy)
		display[,3] <- ifelse(result$penalty > 0, "Yes", "No")
		display[,4] <- sprintf("$%0.2f", result$actearnings)
		display[,5] <- sprintf("%0.2f%%", 100*result$actapy)
		colnames(display) <- c("Product", "Nominal APY<sup>1</sup>", "Early Withdrawal", "Actual Earnings<sup>2</sup>", "Actual APY<sup>2</sup>")

		# Warn user if best rate is worse than inflation
		if (result$actapy[ranking[1]] < inflation) {
			output$message <- renderUI({
				HTML(sprintf("<font color=\"red\">The best actual APY, %0.2f%%, is less than the rate of inflation, %0.2f%%!", 100*result$actapy[ranking[1]], 100*inflation))
			})
		} else {
			output$message <- renderUI({
				div()
			})
		}
		
		# Only show the top 10 rows
		show.rows <- min(10, length(ranking))
		
		if (show.rows == 0) {
			stop("No investments matched your criteria.")
		}
		
		display[ranking[1:show.rows],]
	},
	include.rownames = FALSE,
	sanitize.text.function = function(x) x,
	sanitize.colnames.function = function(x) x)
	
})


