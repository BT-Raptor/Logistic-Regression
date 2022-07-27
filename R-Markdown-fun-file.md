Logistic Regression
================
BT_Raptor
21/04/2022

``` r
require(data.table)
```

    ## Loading required package: data.table

``` r
require(stats)
setwd("C:/Users/ricke/Desktop/Raptor's Data Science/")
options(scipen=999)
```

Data Acquisition

``` r
all_data=fread("Titanic.csv")
all_data=na.omit(all_data)
head(all_data)
```

    ##                                             Name PClass   Age    Sex Survived
    ## 1:                  Allen, Miss Elisabeth Walton    1st 29.00 female        1
    ## 2:                   Allison, Miss Helen Loraine    1st  2.00 female        0
    ## 3:           Allison, Mr Hudson Joshua Creighton    1st 30.00   male        0
    ## 4: Allison, Mrs Hudson JC (Bessie Waldo Daniels)    1st 25.00 female        0
    ## 5:                 Allison, Master Hudson Trevor    1st  0.92   male        1
    ## 6:                            Anderson, Mr Harry    1st 47.00   male        1

Creating a new dataframe, where we do the column tweaking

``` r
all_data[,Sex_M:=0]
all_data[Sex=='male',Sex_M:=1]
all_data[,PClass_1:=0]
all_data[PClass=='1st',PClass_1:=1]
all_data[,PClass_2:=0]
all_data[PClass=='2nd',PClass_2:=1]
all_data[,PClass_3:=0]
all_data[PClass=='3rd',PClass_3:=1]
all_data$Survived=factor(all_data$Survived)
all_data=all_data[,`:=`(Name=NULL,Sex=NULL,PClass=NULL)]
head(all_data)
```

    ##      Age Survived Sex_M PClass_1 PClass_2 PClass_3
    ## 1: 29.00        1     0        1        0        0
    ## 2:  2.00        0     0        1        0        0
    ## 3: 30.00        0     1        1        0        0
    ## 4: 25.00        0     0        1        0        0
    ## 5:  0.92        1     1        1        0        0
    ## 6: 47.00        1     1        1        0        0

Training & Test Split

``` r
train_data=all_data[seq(1,52),]
test_data=all_data[seq(53,104),]
test_actuals=test_data[,c('Survived')]
test_data[,Survived:=NULL]
```

Running the Model

``` r
model=glm(Survived~.,family='binomial',data=train_data)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ ., family = "binomial", data = train_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4599  -0.9437   0.3856   0.4757   1.4437  
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.00480    1.31949   2.277 0.022772 *  
    ## Age         -0.01453    0.02510  -0.579 0.562658    
    ## Sex_M       -2.92895    0.86331  -3.393 0.000692 ***
    ## PClass_1          NA         NA      NA       NA    
    ## PClass_2          NA         NA      NA       NA    
    ## PClass_3          NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 68.271  on 51  degrees of freedom
    ## Residual deviance: 50.945  on 49  degrees of freedom
    ## AIC: 56.945
    ## 
    ## Number of Fisher Scoring iterations: 5

Examining the Predictions

``` r
preds=predict(model,test_data,type='response')
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
test_actuals$preds=round(preds)
head(test_actuals,10)
```

    ##     Survived preds
    ##  1:        1     1
    ##  2:        1     1
    ##  3:        0     0
    ##  4:        1     1
    ##  5:        1     1
    ##  6:        0     0
    ##  7:        1     1
    ##  8:        1     0
    ##  9:        0     0
    ## 10:        1     1
