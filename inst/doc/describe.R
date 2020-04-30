## ------------------------------------------------------------------------
require(cheese)

heart_disease %>%
  univariate_table()


## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    numeric_summary = 
      c(
        Summary = "mean [sd] / median"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    numeric_summary = 
      c(
        NewSummary = "mean [sd] / median"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    numeric_summary = 
      c(
        `Numeric only` = "mean [sd] / median",
        Summary = "median (q1, q3)"
      ),
    categorical_summary = 
      c(
        Summary = "count",
        `Categorical only` = "percent = 100 * proportion"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    categorical_types = NULL, #Easily disable categorical data from being summarized
    numeric_summary =
      c(
        `Median (Q1, Q3)` = "median (q1, q3)",
        `Min-Max` = "min - max",
        `Mean (SD)` = "mean (sd)"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    all_summary = 
      c(
        `# obs. non-missing` = "available of length"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ Sex
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = Sex ~ 1
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = Sex ~ HeartDisease
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ Sex + HeartDisease
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ HeartDisease + Sex
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = HeartDisease + Sex ~ 1
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ Sex + HeartDisease,
    numeric_summary = 
      c(
        `Mean (SD)` = "mean (sd)"
      ),
    categorical_summary = 
      c(
        `Count (%)` = "count (percent%)"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ Sex,
    add_n = TRUE
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ Sex + HeartDisease,
    add_n = TRUE
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = Sex ~ HeartDisease,
    add_n = TRUE
  )

## ------------------------------------------------------------------------
#Function for a p-value
pval <-
  function(y, x) {
    
    #For categorical data use Fisher's Exact test
    if(some_type(x, "factor")) {
      
      p <- fisher.test(factor(y), factor(x), simulate.p.value = TRUE)$p.value
    
    #Otherwise use Kruskall-Wallis
    } else {
      
      p <- kruskal.test(x, factor(y))$p.value
      
    }
    
    ifelse(p < 0.001, "<0.001", as.character(round(p, 2)))
    
  }


## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ HeartDisease,
    associations = list(`P-value` = pval)
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = ~ Sex + HeartDisease,
    associations = list(`P-value` = pval)
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    strata = Sex ~ HeartDisease,
    associations = list(`P-value` = pval)
  )

## ------------------------------------------------------------------------
heart_disease %>%
  dplyr::mutate(
    BloodSugar = factor(BloodSugar)
  ) %>%
  univariate_table()

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    categorical_types = c("factor", "logical")
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    f_other = list(count = function(x) table(x)),
    other_summary = 
      c(
        Summary = "count"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    categorical_types = NULL,
    f_numeric =
      list(
        cv = ~sd(.x) / mean(.x)
      ),
    numeric_summary = 
      c(
        `Coef. of variation` = "sd / mean = cv"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    format = "none"
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    labels = 
      c(
        Age = "Age (years)",
        ChestPain = "Chest pain"
      ),
    levels = 
      list(
        Sex =
          c(
            Male = "M"
          )
      ),
    order = 
      c(
        "BP",
        "Age",
        "Cholesterol"
      )
  )

## ------------------------------------------------------------------------
heart_disease %>%
  univariate_table(
    variableName = "THESE ARE VARIABLES",
    levelName = "THESE ARE LEVELS",
    fill_blanks = "BLANK",
    caption = "HERE IS MY CAPTION"
  )

