
library(formatR)
setwd("~/Documents/GitHub/FRMQ/Rcodes")
tidy_source(source="Causality_test_AM.R",file="FRMQcausalityAM.R")
tidy_source(source="Causality_test_EU.R",file="FRMQcausalityEU.R")
tidy_source(source="correlation test_AM.R",file="FRMQcorrelationAM.R")
tidy_source(file="FRMQcorrelationAM.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQcorrelationEU.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))

tidy_source(file="FRMQdefineFRMs.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQFRMdistribution.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQlambdaDistribution.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQlambdaboxplot.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQRecessionData.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQRecDensity.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQRecOne.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))
tidy_source(file="FRMQRecMulti.R",indent = getOption("formatR.indent", 4), width.cutoff = getOption("width",80))

