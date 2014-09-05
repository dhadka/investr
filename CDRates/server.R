library(rjson)
library(XML)
library(RCurl)

options <- list()

#Barclays
result <- getURL("https://www.banking.barclaysus.com/svlt/rates.json")
json <- fromJSON(result)

for (product in json$products) {
	if (product$type == "3500") {
		options <- append(options, list(list(provider="Barclays", name=sprintf("Barclays %d Month CD", as.numeric(product$term)), term=as.numeric(product$term), rate=as.numeric(product$rate), apy=as.numeric(product$apy))))
	} else if (product$type == "3000") {
		options <- append(options, list(list(provider="Barclays", name="Barclays Savings", term=0, rate=as.numeric(product$rate), apy=as.numeric(product$apy))))
	}
}

#Ally
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
			
			options <- append(options, list(list(provider="Ally", name=sprintf("Ally %d Month CD", term), term=term, rate=rate, apy=apy)))
		}
	} else if (xmlValue(title) == "Online Savings") {
		description <- xmlElementsByTagName(item, "description")[[1]]
		table <- readHTMLTable(xmlValue(description))[[1]]
		
		for (i in 1:nrow(table)) {
			rate <- as.character(table[i,1])
			rate <- as.numeric(substring(rate, 0, nchar(rate)-1)) / 100
			apy <- as.character(table[i,2])
			apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
			
			options <- append(options, list(list(provider="Ally", name="Ally Savings", term=0, rate=rate, apy=apy)))
		}
	}
}

#American Express
result <- getURL("https://personalsavings.americanexpress.com/rates.json")
json <- fromJSON(result)

for (product in json$products) {
	if (as.numeric(product$type) > 3500 && as.numeric(product$type) < 3600) {
		options <- append(options, list(list(provider="American Express", name=sprintf("American Express %d Month CD", as.numeric(product$term)), term=as.numeric(product$term), rate=as.numeric(product$rate)/100, apy=as.numeric(product$apy)/100)))
	} else if (product$type == "3200") {
		options <- append(options, list(list(provider="American Express", name="American Express Savings", term=0, rate=as.numeric(product$rate)/100, apy=as.numeric(product$apy)/100)))
	}
}

#EverBank
result <- getURL("https://www.everbank.com/banking/cd")
table <- readHTMLTable(result)[[1]]

for (i in 1:nrow(table)) {
	term <- unlist(strsplit(as.character(table[i,1]), "\\s"))
	term <- as.numeric(term[1]) * ifelse(term[2] == "month", 1, 12)
	rate <- as.character(table[i,2])
	rate <- as.numeric(substring(rate, 0, nchar(rate)-1)) / 100
	apy <- as.character(table[i,3])
	apy <- as.numeric(substring(apy, 0, nchar(apy)-1)) / 100
	
	options <- append(options, list(list(provider="EverBank", name=sprintf("EverBank %d Month CD", term), term=term, rate=rate, apy=apy)))
}

# Convert to data frame
options <- data.frame(
	provider=sapply(options, function(o) o$provider),
	name=sapply(options, function(o) o$name),
	term=sapply(options, function(o) o$term),
	rate=sapply(options, function(o) o$rate),
	apy=sapply(options, function(o) o$apy),
	stringsAsFactors=FALSE)

minimums=list(
	"Barclays"=0,
	"Ally"=0,
	"American Express"=0,
	"EverBank"=1500)

penalties=list(
	"Barclays"=function(n) ifelse(n <= 24, 90, 180),
	"Ally"=function(n) ifelse(n <= 24, 60, 30*n/12),
	"American Express"=function(n) n,
	"EverBank"=function(n) 360/12 * n/4)

calc.earnings <- function(option, held, principal) {
	principal*(1+option$rate/365)^(held*365) - ifelse((option$term > 0) && ((12*held) %% option$term > 0), principal*(1+option$rate/365)^penalties[[option$provider]](option$term), principal)
}

calc.estapy <- function(option, held, principal) {
	calc.earnings(option, held, principal)/principal/held
}

federal <- 0.25
state <- 0.0307
local <- 0.0235

calc.actapy <- function(option, held, principal) {
	calc.earnings(option, held, principal)*(1-(federal+state+local))/principal/held
}

earnings <- sapply(1:nrow(options), function(i) calc.earnings(options[i,], 1, 1000))
estapy <- sapply(1:nrow(options), function(i) calc.estapy(options[i,], 1, 1000))
actapy <- sapply(1:nrow(options), function(i) calc.actapy(options[i,], 1, 1000))

result <- data.frame(options, earnings, estapy, actapy)
print(result)