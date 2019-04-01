## ------------------------------------------------------------------------
#Load packages
require(tidyverse); require(cheese)

#Look at the top ten rows
heart_disease


## ------------------------------------------------------------------------
#Default table
heart_disease %>%
    univariate_table

## ------------------------------------------------------------------------
#Single column strata
heart_disease %>%
    univariate_table(
        strata = ~HeartDisease
    )
heart_disease %>%
    univariate_table(
        strata = ~HeartDisease,
        add_n = TRUE
    )

#Multiple column strata
heart_disease %>%
    univariate_table(
        strata = ~Sex + HeartDisease
    )
heart_disease %>%
    univariate_table(
        strata = ~Sex + HeartDisease,
        add_n = TRUE,
        strata_sep = "|"
    )

#Single row strata
heart_disease %>%
    univariate_table(
        strata = Sex~1
    )
heart_disease %>%
    univariate_table(
        strata = Sex~1,
        add_n = TRUE
    )

#Column and row strata
heart_disease %>%
    univariate_table(
        strata = Sex~HeartDisease
    )
heart_disease %>%
    univariate_table(
        strata = Sex~HeartDisease,
        add_n = TRUE
    )


## ------------------------------------------------------------------------

#Define functions to add to table
pvalues <- 
    function(
        y, #Will be the strata variable
        x #Each other variable
    ) {
        
        #Different tests depending on type
        if(is(x, "numeric")) {
            wilcox.test(x~y)$p.value
        } else {
            fisher.test(table(x, y))$p.value
        }
        
    }

#Supply function to table
heart_disease %>%
    univariate_table(
        strata = ~HeartDisease,
        add_n = TRUE,
        associations = pvalues
    )

#Make a named list to name the column
metrics <- list(`P-value` = pvalues)
heart_disease %>%
    univariate_table(
        strata = ~HeartDisease,
        add_n = TRUE,
        associations = metrics
    )

#Add additional function to list that computes the AIC of a logistic regression model
metrics$AIC <- 
    function(y, x) AIC(glm(factor(y)~x, family = "binomial"))
heart_disease %>%
    univariate_table(
        strata = ~HeartDisease,
        add_n = TRUE,
        associations = metrics
    )

#Compute metrics across salary within sex
heart_disease %>%
    univariate_table(
        strata = Sex~HeartDisease,
        add_n = TRUE,
        associations = metrics
    )


## ------------------------------------------------------------------------

#Add summary columns for numeric data
heart_disease %>%
    univariate_table(
        numeric_summary = c(Median = "median", Mean = "mean")
    )

#Add a stratification variable
heart_disease %>%
    univariate_table(
        numeric_summary = c(Median = "median", Mean = "mean"),
        strata = ~HeartDisease
    )


## ------------------------------------------------------------------------
heart_disease %>%
    divide(
        by = "Sex"
    )

## ------------------------------------------------------------------------
heart_disease %>%
    stratiply(
        strata = c("Sex", "HeartDisease"),
        f = function(x) 
            x %>% 
            select_if(is.numeric) %>% 
            map(mean, na.rm = TRUE),
        bind = TRUE,
        separate = TRUE
    )   


## ------------------------------------------------------------------------
#Create a frame of summaries
temp_summary <-
    
    heart_disease %>%
    group_by(
        Sex,
        HeartDisease,
        BloodSugar
    ) %>%
    summarise(
        Mean = mean(Age, na.rm = TRUE),
        SD = sd(Age, na.rm = TRUE),
        Median = median(Age, na.rm = TRUE)
    ) %>%
    ungroup() 

#Span summaries for each combination of Sex and BloodSugar
temp_summary %>%
    stretch(
        keys = c("Sex", "BloodSugar"),
        keep = "HeartDisease"
    )

#Clean HTML table with keys spanned over columns
result <- 
    temp_summary %>%
    stretch(
        keys = c("Sex", "BloodSugar"),
        keep = "HeartDisease",
        extract_keys_as_header = TRUE,
        keep_keys_in_header = FALSE
    )
result$.result %>%
    knitr::kable(format = "html") %>%
    kableExtra::kable_styling() %>%
    kableExtra::add_header_above(
        kableExtra::auto_index(result$.header)
    )
    


## ------------------------------------------------------------------------
heart_disease %>%
    dish(
        f =
            function(y, x) {
                mod <- lm(y ~ x)
                tibble(
                    Parameter = names(mod$coef),
                    Estimate = mod$coef
                )
            },
        left = c("Age", "BP"),
        bind = TRUE
    )


## ------------------------------------------------------------------------
absorb(
    key = c("mean", "sd", "var"),
    value = c("10", "2", "4"),
    text = 
        c("MEAN: mean, SD: sd",
          "VAR: var = sd^2",
          MEAN = "mean"
        )
)


## ------------------------------------------------------------------------
heart_disease %>%
    
    #Compute means and medians on numeric data
    typly(
        c("numeric", "logical"),
        list(
            mean = mean,
            median = median
        ),
        keep = TRUE,
        na.rm = TRUE
    ) %>%
    
    #Compute table
    typly(
        "factor",
        table,
        keep = TRUE
    )

## ------------------------------------------------------------------------
heart_disease %>%
    descriptives(
        f_numeric = 
            list(
                cv = function(x, na.rm) sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
            )
    )

## ------------------------------------------------------------------------
#Make a list of functions
f <-
    list(
        
        #Compute a univariate p-value
        `P-value` =
            
            function(x, y) {
                
                if(type_match(y, c("factor", "character"))) {
                    
                    p <- fisher.test(factor(x), factor(y), simulate.p.value = TRUE)$p.value
                    
                    
                } else {
                    
                    p <- kruskal.test(y, factor(x))$p.value
                    
                }
                if_else(
                    p < 0.001, "<0.001", as.character(round(p, 2))
                )
            },
        
        #Compute difference in AIC model between null model and one predictor model
        `AIC Difference` =
            function(x, y) {
                
                glm(factor(x)~1, family = "binomial")$aic - 
                glm(factor(x)~y, family = "binomial")$aic
                
            }
    )

#1) Apply functions to Sex/HeartDisease by all other variables
heart_disease %>%
    univariate_associations(
        f = f,
        responses = c("Sex", "HeartDisease")
    )

