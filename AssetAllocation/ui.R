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
	titlePanel("Asset Allocations & Rebalancing"),
	sidebarLayout(
		sidebarPanel(
			helpText("Input your ticker symbols and percentages below.  The percentages will be scaled to 100% if they sum to less/more than 100%."),
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
			#selectInput("duration", "Duration", c(
			#	"1 Year"=365,
			#	"2 Years"=2*365,
			#	"5 Years"=5*365,
			#	"10 Years"=10*365,
			#	"25 Years"=25*365),
			#	25*365),
			checkboxInput("rebalance", "Rebalance yearly", value=TRUE)
			),
		mainPanel(
			plotOutput("plot"),
			tableOutput("table")))))