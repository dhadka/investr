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
library(corrgram)
library(stringr)

cached.data <- lapply(cached.tickers, function(ticker) getSymbols(ticker, from=Sys.Date()-10*365, to=Sys.Date(), auto.assign=FALSE, warnings=FALSE))
names(cached.data) <- cached.tickers

shinyServer(function(input, output, session) {
	observe({
		query <- parseQueryString(session$clientData$url_search)
		
		if (!is.null(query$q)) {
			updateTextInput(session, "custom", value=query$q)
		}
		
		if (!is.null(query$uncheck)) {
			updateCheckboxGroupInput(session, "tickers", selected=list())
		}
	})
	
	loadCachedTickers <- reactive({
		if (is.null(input$tickers)) {
			list()
		} else {
			lapply(input$tickers, function(t) {
				result <- Ad(cached.data[[t]])
				names(result) <- t
				result
			})
		}
	})
	
	loadCustomTickers <- reactive({
		result.list <- list()
		
		if (!is.null(input$custom)) {
			if (input$duration == "custom") {
				start <- input$date[1]
				end <- input$date[2]
			} else {
				duration <- as.numeric(input$duration)
				start <- Sys.Date()-duration
				end <- Sys.Date()
			}
			
			custom.tickers <- unlist(strsplit(input$custom, "\\s*[,\\s]\\s*", perl=TRUE))
			
			for (t in custom.tickers) {
				t <- str_trim(t)
				custom.data <- tryCatch(getSymbols(t, from=start, to=end, auto.assign=FALSE, warnings=FALSE), error=function(e) { return(NULL) })
				
				if (!is.null(custom.data)) {
					result <- Ad(custom.data)
					names(result) <- t
					result.list <- c(result.list, list(result))
				}
			}
		}
		
		result.list
	})
	
	loadTickers <- reactive({
		merged.tickers <- c(loadCachedTickers(), loadCustomTickers())
		
		if (length(merged.tickers) < 2) {
			stop("Provide at least two tickers.")
		}
		
		merged <- do.call(merge, merged.tickers)
		na.locf(merged)
	})
	
	output$plot <- renderPlot({
		data <- loadTickers()
		
		if (input$duration == "custom") {
			subset <- window(data, start=input$date[1], end=input$date[2])
		} else {
			duration <- as.numeric(input$duration)
			subset <- window(data, start=Sys.Date()-duration, end=Sys.Date())
		}

		# calculate returns based on fixed data
		returns <- do.call(merge, lapply(1:ncol(subset), function(i) ROC(subset[,i])))
		
		returns <- as.matrix(returns)
		colnames(returns) <- colnames(data)
		corrgram(as.matrix(returns), order=NULL, lower.panel=panel.shade,
				 upper.panel=NULL, text.panel=panel.txt, main="",
				 col.regions=palette)
	})
	
	output$legend <- renderPlot({
		oldpar <- par(no.readonly=TRUE)
		par(mai=c(1, 2, 0.5, 2))
		crange <- range(-1, 1)
			
		image(seq(crange[1], crange[2], (crange[2]-crange[1])/100), 0, matrix(seq(0, 1, 0.01), ncol=1), col=palette(100), axes=FALSE, xlab="Correlation", ylab="")
		box("plot")
		axis(1, at=c(-1, 0, 1), labels=c("Negatively Correlated", "Uncorrelated", "Positvely Correlated"))
		
		par(oldpar)
	})
})
