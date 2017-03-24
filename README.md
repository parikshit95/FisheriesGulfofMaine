# FisheriesGulfofMaine

Natural and social phenomena are characterized by a high level of complexity and multiple factors that
tend to be highly correlated. Overfishing in the Gulf of Maine, and its impact on the fisheries that once
existed in the Gulf is one such phenomenon. 
This research project uses statistical analysis to estimate the
likelihood that a particular region in the Gulf of Maine was a fishing spot in the past. In addition, the
analysis enables us to estimate the many factors, such as, seasonality and the cost of fishing, which
affected the productivity of historic Gulf of Maine fishing spots.

The project, which we have titled “Explaining the presence and productivity of historic fishing grounds
in the Gulf of Maine,” builds on the insight and information provided by the last few generations of
fishermen who made a living catching fish in the Gulf. The data was collected by Ted Ames, a
Bowdoin Coastal Scholar. Over the years Ted has interviewed retired fishermen who once caught hake,
haddock, and cod. Furthermore, Professor Eileen Johnson and several Bowdoin students have made this
data amendable for statistical analysis by digitizing the data that Ted collected. They created a map that
indicates the boundaries of the historical fishing sites and their productivity across species and seasons.
The likelihood that a particular region in the gulf of Maine was a fishing spot in the past can be
expressed mathematically using a relation that maps a set of explanatory factors to their likelihood. This
set of explanatory factors or variable includes mean water depth (in meters), mean ocean floor slope,
distance to the shore (in kilometres) and distance to a river (in kilometres). As part of my work on this
project, I explored ways to make the fishing model as accurate as possible. For example, using the
square of the explanatory variables, say distance to shore squared, leads to a result that implies that at
first additional depth increases the likelihood of a spot being a good fishing spot, but, at some point
more depth becomes disadvantageous. This is more realistic because some depth and distance is needed
to find fish in a boat.

However, the high level of correlation within the set of explanatory variables makes it essential that the
estimation is rigorous and that the model is robust enough to capture the complexity of the biophysical
phenomena observed in the case of fishing spots. For instance, if the model omits one or more spatially
correlated processes that affect fishing site productivity then the standard errors will be biased. Thus,
one of my main aims is to explore statistical means to check and reduce the amount of bias and standard
error that might be observed in the analysis.

This research was materialized and effectively executed through the generous support of
Grua/O’Connell Fund. The research project was a great opportunity to gain insight and experience in the
field of economics and applied mathematics. The project enabled me to build on coursework and
academic perspective from my Mathematics and Economics majors in a new academic setting, beyond
the classroom. Furthermore, the focus on fishing spots in the Gulf of Maine, helped me link my
academic scholarship to cultural and natural facets of the state of Maine, thereby enhancing my
understanding of the place I am grateful for the kind consideration and support provided by Professor
Nelson and the Grua/O’Connell Fund.

During the course of research I could also take a mile deep dive into machine learning techniques to
answer some of questions stated above as well as to introduce a new approach to data driven
investigations. These techniques move beyond traditional regression estimations where all else is held
constant and address the determinants of a dependent variable all factors considered. The following
figure visualizes the region of focus. The data at hand contains around 2.6 million coordinates and 21
bio-physical and economically relevant variables such as distance to port which can be used as a proxy
for valuation of fisheries by fishermen. The results reported in this document are just for region 5 but a
similar methodology was applied for all regions.

## Description of files
All code is included in this document. The following is a list of R files and their functions:
1. Code5.R contains packages for running and interpretting logistic regressions on the data for region 5
2. SubSample.R contains packages and examples for carryign robustness checks on the data
3. Rpart.R contains packages for running a recursive partitioning library rpart on the data to make classification trees
4. main.R contains packages and code to run the entire project for all regions across all seasons.
