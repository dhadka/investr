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
	titlePanel("Guaranteed Income Comparison"),
	sidebarLayout(
		sidebarPanel(
			h3("Options"),
			sliderInput("principal", "Principal", min=1000, max=10000, value=1000, step=1000),
			sliderInput("held", "Investment Duration (Years)", min=1, max=10, value=1, step=1),
			br(),
			checkboxInput("taxable", "Is Taxable?", TRUE),
			conditionalPanel("input.taxable",
				wellPanel(
					numericInput("federal", "Federal Marginal Tax Rate", 25, min=0, max=100, step=1),
					numericInput("state", "State Tax Rate", 3.07, min=0, max=100, step=0.01),
					numericInput("local", "Local Tax Rate", 2.35, min=0, max=100, step=0.01))),
			br(),
			h4("Help"),
			helpText("Based on your selected options, the table to the right
					 shows the 10 highest-yield guaranteed income investments.
					 Only nationally available, FDIC-insured banks and US-backed
					 bonds are considered.")),
		mainPanel(
			conditionalPanel("!output.table", p("Loading data, please wait...")),
			tableOutput("table"),
			htmlOutput("message"))),
	HTML("<sup>1</sup> Nominal APY is the APY you would realize if the investment
		 is held to maturity.<br>
		 <sup>2</sup> Actual earnings and actual APY account for early
		 withdrawal penalties and taxes.  If the term is less than your investment
		 duration, we assume you renew with the same interest rate.<br>
		 <sup>3</sup> Federal savings bonds and notes are exempt from local and state tax.
		 I bonds must be held for at least one year after purchase.  The APY is
		 tied to the inflation rate, and may increase or decrease in the future."),
	br(),
	br(),
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