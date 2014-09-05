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

shinyUI(fluidPage(
	titlePanel("Correlations"),
	sidebarLayout(
		sidebarPanel(
			selectInput("duration", "Duration", c(
				"1 Month"=30,
				"3 Months"=91,
				"6 Months"=182,
				"1 Year"=365,
				"2 Years"=2*365,
				"3 Years"=3*365,
				"4 Years"=4*365,
				"5 Years"=5*365,
				"10 Years"=10*365,
				"Custom Range"="custom")),
			conditionalPanel("input.duration === 'custom'",
				dateRangeInput("date", "Date Range", start=Sys.Date()-365, end=Sys.Date())),
			checkboxGroupInput("tickers", "Tickers", cached.tickers, cached.tickers),
			textInput("custom", "Custom Ticker")),
		mainPanel(
			div(
				plotOutput("plot", height="500px"),
				plotOutput("legend", height="150px"),
				style="width: 800px; margin: 0px auto;")
			)),
	em("Investing entails some degree of risk.  Investors should learn the risks
		 involved before engaging in any investment.  Investr provides general
		 information, not individually targeted personalized advice.  Some
		 investment strategies may not be suitable for you.  We recommend you seek
		 independent professional legal, tax, and investment advice before engaging
		 in any investment.  Information and advice provided by Investr should not
		 be construed as an offer to sell, a solicitation of an offer to buy, or a
		 recommendation for any investment."),
	br(),
	br(),
	em("Past performance does not necessarily predict future returns.  We
		 expressly deny any liability to you for loss in any manner or form now or
		 at any time in the future.  Investr endeavours to ensure the accuracy and
		 reliability of the information provided, but does not accept any liability
		 whatsoever, whether in tort or contract or otherwise, for any loss or
		 damage arising from the use of this software.")))