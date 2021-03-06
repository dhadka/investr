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
	titlePanel("Rebalancing"),
	sidebarLayout(
		sidebarPanel(
			helpText("Input your ticker symbols and percentages below."),
			tags$table(
				tags$tr(
					tags$th("Ticker"),
					tags$th("Percent")),
				tags$tr(
					tags$td(tags$input(id="ticker.1", type="text", value="FUSEX", class="input-small")),
					tags$td(tags$input(id="percent.1", type="text", value="60", class="input-small"))),
				tags$tr(
					tags$td(tags$input(id="ticker.2", type="text", value="FBNDX", class="input-small")),
					tags$td(tags$input(id="percent.2", type="text", value="20", class="input-small"))),
				tags$tr(
					tags$td(tags$input(id="ticker.3", type="text", value="FRESX", class="input-small")),
					tags$td(tags$input(id="percent.3", type="text", value="10", class="input-small"))),
				tags$tr(
					tags$td(tags$input(id="ticker.4", type="text", value="FIGRX", class="input-small")),
					tags$td(tags$input(id="percent.4", type="text", value="10", class="input-small"))),
				tags$tr(
					tags$td(tags$input(id="ticker.5", type="text", value="", class="input-small")),
					tags$td(tags$input(id="percent.5", type="text", value="", class="input-small"))),
				tags$tr(
					tags$td(tags$input(id="ticker.6", type="text", value="", class="input-small")),
					tags$td(tags$input(id="percent.6", type="text", value="", class="input-small")))),
			br(),
			br(),
			h4("Help"),
			helpText("Rebalancing is a means to maintain your portfolio's risk as the
							 percentage of each asset changes over time.  The chart to the
							 right shows the impact of different rebalancing frequencies on
							 your portfolio.")
			),
		mainPanel(
			conditionalPanel("!output.plot", p("Loading data, please wait...")),
			plotOutput("plot"),
			tableOutput("table"))),
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