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
	titlePanel("Asset Class/Sector Performance"),
	sidebarLayout(
		sidebarPanel(
			h3("Options"),
			selectInput("view", "View Asset Class or Stock Sector", c("Class", "Sector"), "Class"),
			checkboxInput("cumulative", "Show cumulative dates", FALSE),
			checkboxInput("plot", "Show as plot", FALSE),
			br(),
			br(),
			h4("Help"),
			conditionalPanel("input.plot", helpText(
					 "This plot shows the percent change for each asset class or sector
					 over a rolling 1 year period.  For example, the largest peak in this
					 graph occurs during the summer of 2009.  This peak reflects the
					 increase in value starting 6 months prior (January 2009) to 6 months
					 after (December 2009) the indicated date.")),
			conditionalPanel("!input.plot", helpText(
					 "This table identifies which asset class or sector performed best
					 during each time period (the top row) and which performed worst
					 (bottom row).  Each cell in the table indicates the asset class /
					 sector and the percent change during the given time period."))),
		mainPanel(
			conditionalPanel("!output.ui", p("Loading data, please wait...")),
			uiOutput("ui")))))