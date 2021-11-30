# Questions 

## PLS Data 

### 1. What are the attributes we want to extract to use in our model? Current "names" in pls1 are: 

    [1] "coefficients"    "scores"          "loadings"        "loading.weights" "Yscores"         "Yloadings"      

    [7] "projection"      "Xmeans"          "Ymeans"          "fitted.values"   "residuals"       "Xvar"           

    [13] "Xtotvar"         "fit.time"        "ncomp"           "method"          "validation"      "call"           

    [19] "terms"           "model"          

-> For most of these "headers" there are multiple subgroups. Are we trying to extract specific subgroups of the abover attributes? 

### 2. How are we wanting to create a loading plot? The loading plot in the manual example plots BSi values with ncomp on x-axis and RMSEP on y-axis. 

* I don't know how to extract ncomp and RMSEP to create a plot like ggplot(data=pls1, aes(x=ncomp, y=RMSEP)) + geom_line