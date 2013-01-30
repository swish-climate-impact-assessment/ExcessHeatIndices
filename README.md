
Excess Heat Indices     
-------------------

During 2011 I worked for Geoff Morgan (Geoff.Morgan@ncahs.health.nsw.gov.au) on a consultancy with NSW health to look at heatwaves, mortality and admissions. We use the percentiles of daily max temperature and apparent temperature in a similar way to the paper by Behnoosh Khalaj and Keith Dear. In additional sensitivity analyses we also developed material related to a newly proposed heatwave metric called the Excess Heat Factor by John Nairn at the BoM.

The reports/EHIs_transformations_doc.Rnw file is an Sweave document which contains the complete text and R codes that you can execute and produce the PDF (also found in the reports directory).  The interested reader is encouraged to run the R codes to do the calculations and generate the graphs that get compiled into that pdf file.  These R codes are also held separately in the src directory and can be evaluated in the correct sequence using the go.r script if you prefer.  Please don't hesitate to send me queries or comments on the algorithms or other aspects of this work.

Some Background
---------------

We were asked by our NSW health collaborators to investigate some heatwave indices developed by the BoM. NSW BoM like the look of three indices invented at the SA BoM office (by John Nairn) - they want to construct a national definition. Apparently BoM central HQ like John's definition the most (not published in a journal yet, the best ref is http://www.cawcr.gov.au/events/modelling_workshops/workshop_2009/papers/NAIRN.pdf). 

John has worked with PriceWaterhouseCoopers to apply the heatwave in a recent report http://www.pwc.com.au/industry/government/assets/extreme-heat-events-nov11.pdf

Ivan Hanigan
2012-04-21
