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

palette <- colorRampPalette(c("navy", "royalblue", "white", "salmon", "red"))

cached.tickers <- c("TIP - iShares Barclays TIPS Bond Fund"="TIP",
					"GLD - SPDR Gold Shares"="GLD",
					"AGG - iShares Barclays Aggregate Bond"="AGG",
					"EMB - iShares JPMorgan USD Emerging Markets Bond"="EMB",
					"USO - United States Oil"="USO",
					"GSG - iShares S&P GSCI Commodity-Indexed Trust"="GSG",
					"VNQ - Vanguard REIT Index ETF"="VNQ",
					"RWX - SPDR Dow Jones Intl Real Estate"="RWX",
					"EEM - iShares MSCI Emerging Markets Index"="EEM",
					"EFA - iShares MSCI EAFE Index"="EFA",
					"VB - Vanguard Small Cap ETF"="VB",
					"VV - Vanguard Large Cap ETF"="VV",
					"VO - Vanguard Mid-Cap ETF"="VO")