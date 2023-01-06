## -----------------------------------------------------------------------------
#Load the package
require(cheese)

## -----------------------------------------------------------------------------
#Look at the data
heart_disease

## -----------------------------------------------------------------------------
div_dat <-
  heart_disease %>%
    divide(
      Sex,
      HeartDisease
    )
div_dat

## -----------------------------------------------------------------------------
div_dat %>%
  fasten()

## -----------------------------------------------------------------------------
div_dat %>%
  fasten(
    into = c("Sex", "HeartDisease")
  )

## -----------------------------------------------------------------------------
div_dat %>%
  fasten(
    into = "Sex"
  )

## -----------------------------------------------------------------------------
div_dat %>%
  fasten(
    into = c("", "HeartDisease")
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  divide(
    Sex,
    HeartDisease,
    depth = 1
  )

## -----------------------------------------------------------------------------
div_dat %>%
  fasten(
    into = "HeartDisease",
    depth = 1
  )

## -----------------------------------------------------------------------------
div_dat %>%
  fasten(
    into = "HeartDisease",
    depth = -1
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  dish(
    f = cor,
    left = c(BP, Cholesterol),
    right = where(is.numeric)
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  dplyr::select(where(is.numeric)) %>% #Need to do this so only numeric columns are evaluated
  dish(
    f = cor,
    left = c(BP, Cholesterol)
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  dish(
    f = function(y, x) lm(y ~ ., data = x),
    left = c(BP, Cholesterol),
    each_right = FALSE
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  stratiply(
    f = glm,
    by = Sex,
    formula = HeartDisease ~ . -ChestPain,
    family = "binomial"
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  stratiply(
    f = glm,
    by = c(Sex, ExerciseInducedAngina),
    formula = HeartDisease ~ . -ChestPain,
    family = "binomial"
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  typly(
    f = mean,
    types = c("numeric", "logical")
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  typly(
    f = table,
    types = c("numeric", "logical"),
    negated = TRUE,
    useNA = "always"
  )

## -----------------------------------------------------------------------------
absorb(
  key = c("mean", "sd", "var"),
  value = c("10", "2", "4"),
  text = 
    c(
      "MEAN: mean, SD: sd",
      "VAR: var = sd^2",
      MEAN = "mean"
    )
)

## -----------------------------------------------------------------------------
age_stats <-
  heart_disease %>%
  dplyr::group_by(
    ExerciseInducedAngina,
    HeartDisease
  ) %>%
  dplyr::summarise(
    mean = round(mean(Age), 2),
    sd = round(sd(Age), 2),
    median = median(Age),
    iqr = IQR(Age),
    .groups = "drop"
  ) %>%
  tidyr::gather(
    key = "key",
    value = "value",
    -ExerciseInducedAngina,
    -HeartDisease
  )
age_stats

## -----------------------------------------------------------------------------
age_stats %>%
  dplyr::group_by(
    ExerciseInducedAngina,
    HeartDisease
  ) %>%
  dplyr::summarise(
    Summary = 
      absorb(
        key = key,
        value = value,
        text = 
          c(
            "mean (sd) | median (iqr)"
          )
      ),
    .groups = "drop"
  ) 

## -----------------------------------------------------------------------------
absorb(
  key = age_stats$key,
  value = age_stats$value,
  text = c("(mean) / (sd)", "median")
)

## -----------------------------------------------------------------------------
absorb(
  key = age_stats$key,
  value = age_stats$value,
  text = c("(mean) / (sd)", "median"),
  sep = "+",
  evaluate = TRUE,
  trace = TRUE
)

## -----------------------------------------------------------------------------
#Make a divided data frame
div_dat1 <-
  heart_disease %>%
  divide(
    Sex,
    HeartDisease
  )

#Find the depths of the data frames
div_dat1 %>%
  depths(
    predicate = is.data.frame
  )
div_dat1 %>%
  depths_string(
    predicate = is.data.frame
  )

## -----------------------------------------------------------------------------
div_dat1 %>%
  depths_string(
    predicate = is.data.frame,
    bare = FALSE
  )

## -----------------------------------------------------------------------------
set.seed(123)
heart_disease %>%
  muddle()

## -----------------------------------------------------------------------------
heart_disease %>%
  muddle(
    at = Age
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  muddle(
    size = 5
  )

## -----------------------------------------------------------------------------
mod_results <-
  heart_disease %>%
  
  #For each sex
  stratiply(
    by = Sex,
    f =
      ~.x %>%
      
      #Collect coefficients and standard errors for model estimates on multiple outcomes
      dish(
        f = function(y, x) {
          model <- lm(y ~ ., data = x)
          tibble::tibble(
            Parameter = names(model$coefficients),
            Estimate = model$coefficients,
            SE = sqrt(diag(vcov(model)))
          )
        },
        left = c(BP, Cholesterol),
        each_right = FALSE
      ) 
  ) %>%
  
  #Gather results into a single data frame
  fasten(
    into = c("Sex", "Outcome")
  )
mod_results

## -----------------------------------------------------------------------------
straught1 <-
  mod_results %>%
  stretch(
    key = Outcome,
    value = c(Estimate, SE)
  )
straught1

## -----------------------------------------------------------------------------
straught2 <-
  mod_results %>%
  stretch(
    key = c(Sex, Outcome),
    value = c(Estimate, SE)
  )
straught2

## -----------------------------------------------------------------------------
straught1 %>%
  grable(
    at = -c(Sex, Parameter)
  )

## -----------------------------------------------------------------------------
straught2 %>%
  grable(
    at = -Parameter
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  descriptives()

## -----------------------------------------------------------------------------
heart_disease %>%
  univariate_associations(
    f = list(AIC = function(y, x) AIC(lm(y ~ x))),
    responses = c(BP, Cholesterol)
  )

## -----------------------------------------------------------------------------
heart_disease %>%
  univariate_table()

