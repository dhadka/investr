library(rjson)
library(XML)
library(RCurl)

#Barclays
result <- getURL("https://www.banking.barclaysus.com/svlt/rates.json")
json <- fromJSON(result)

for (product in json$products) {
	if (product$type == "3500") {
		#CDs
		cat(sprintf("Barclays %d Month CD, %0.2f%% APY\n", as.numeric(product$term), 100*as.numeric(product$apy)))
	} else if (product$type == "3000") {
		#Savings
		cat(sprintf("Barclays Savings, %0.2f%% APY\n", 100*as.numeric(product$apy)))
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
			months <- as.numeric(term[1]) * ifelse(term[2] == "month", 1, 12)
			rate <- as.character(table[i,3])
			rate <- as.numeric(substring(rate, 0, nchar(rate)-1))
			cat(sprintf("Ally %d Month CD, %0.2f%% APY\n", months, rate))
		}
	} else if (xmlValue(title) == "Online Savings") {
		description <- xmlElementsByTagName(item, "description")[[1]]
		table <- readHTMLTable(xmlValue(description))[[1]]
		
		for (i in 1:nrow(table)) {
			rate <- as.character(table[i,2])
			rate <- as.numeric(substring(rate, 0, nchar(rate)-1))
			cat(sprintf("Ally Savings, %0.2f%% APY\n", rate))
		}
	}
}

#American Express
result <- getURL("https://personalsavings.americanexpress.com/rates.json")
json <- fromJSON(result)

for (product in json$products) {
	if (as.numeric(product$type) > 3500 && as.numeric(product$type) < 3600) {
		#CDs
		cat(sprintf("American Express %d Month CD, %0.2f%% APY\n", as.numeric(product$term), as.numeric(product$apy)))
	} else if (product$type == "3200") {
		#Savings
		cat(sprintf("American Express Savings, %0.2f%% APY\n", as.numeric(product$apy)))
	}
}

#EverBank
result <- getURL("https://www.everbank.com/banking/cd")
table <- readHTMLTable(result)[[1]]

for (i in 1:nrow(table)) {
	term <- unlist(strsplit(as.character(table[i,1]), "\\s"))
	months <- as.numeric(term[1]) * ifelse(term[2] == "month", 1, 12)
	rate <- as.character(table[i,3])
	rate <- as.numeric(substring(rate, 0, nchar(rate)-1))
	cat(sprintf("EverBank %d Month CD, %0.2f%% APY\n", months, rate))
}
