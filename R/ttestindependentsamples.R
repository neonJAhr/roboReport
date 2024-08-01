#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

TTestIndependentSamplesInternal <- function(jaspResults, dataset = NULL, options, ...) {
  #at least one variable and one grouping variable
  ready <- length(options$dependent) > 0 && options$group != ""
  type  <- "independent"

  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestIndependentMainTable(  jaspResults, dataset, options, ready, type)
  .ttestIndependentNormalTable(jaspResults, dataset, options, ready, type)
  .ttestQQPlot(                jaspResults, dataset, options, ready, type)
  .ttestIndependentEqVarTable( jaspResults, dataset, options, ready, type)
  # Descriptives
  .ttestIndependentDescriptivesTable(        jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesPlot(         jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesRainCloudPlot(jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesBarPlot(      jaspResults, dataset, options, ready)
  .ttestIndependentDescriptivesRainCloudPlot(jaspResults, dataset, options, ready)
  # RoboReport Texts
  .ttestIndependentIntroText(jaspResults, dataset, options, ready, type)
  .ttestIndependentSummaryText(jaspResults, dataset, options, ready, type)
  .ttestDescriptivesText(jaspResults, dataset, options, ready, type)
  .ttestAssumptionsText(jaspResults, dataset, options, ready, type)
  .ttestParametersText(jaspResults, dataset, options, ready, type)
  .ttestHypothesisText(jaspResults, dataset, options, ready, type)
  .ttestReferences(jaspResults, dataset, options, ready, type)

  return()
}

.ttestIndependentMainTable <- function(jaspResults, dataset, options, ready, type) {
  if (!is.null(jaspResults[["ttest"]]))
    return()

  optionsList <- .ttestOptionsList(options, type)

  # Create table
  ttest <- createJaspTable(title = gettext("Independent Samples T-Test"))
  ttest$dependOn(c("effectSize", "effectSizeCi", "dependent",
                   "effectSizeCiLevel", "student",
                   "meanDifference", "meanDifferenceCi",
                   "meanDifferenceCiLevel", "alternative",
                   "vovkSellke", "naAction", "group", "effectSizeType",
                   "welch", "mannWhitneyU", "equalityOfVariancesTest", "equalityOfVariancesTestType"))
  ttest$showSpecifiedColumnsOnly <- TRUE
  ttest$position <- 1

  if (optionsList$wantsWilcox && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Mann-Whitney U test."))
    testStat <- "U"
    testStatName <- gettext("U")
  } else if (optionsList$wantsWelchs && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Welch's t-test."))
    testStat <- "t"
    testStatName <- gettext("t")
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
    ttest$addFootnote(gettext("Student's t-test."))
    testStat     <- "t"
    testStatName <- gettext("t")
  } else {
    testStat     <- "Statistic"
    testStatName <- gettext("Statistic")
  }

  dfType <- ifelse(optionsList$wantsWelchs, "number", "integer")

  ttest$addColumnInfo(name = "v", title = " ", type = "string", combine = TRUE)

  if (sum(optionsList$allTests) >= 2)
    ttest$addColumnInfo(name = "test", type = "string",  title = gettext("Test"))

  ttest$addColumnInfo(name = testStat, type = "number",  title = testStatName)
  ttest$addColumnInfo(name = "df",     type = dfType,    title = gettext("df"))
  ttest$addColumnInfo(name = "p",      type = "pvalue",  title = gettext("p"))

  .ttestVovkSellke(ttest, options)

  if (options$effectSizeType == "cohen")
    effSize <- "cohen"
  else if (options$effectSizeType == "glass")
    effSize <- "glass"
  else if (options$effectSizeType == "hedges")
    effSize <- "hedges"

  nameOfEffectSizeParametric <- switch(effSize,
                                       cohen  = gettext("Cohen's d"),
                                       glass  = gettext("Glass' delta"),
                                       hedges = gettext("Hedges' g"))

  if (!optionsList$wantsWilcox) {
    nameOfLocationParameter <- gettext("Mean Difference")
    nameOfEffectSize        <- nameOfEffectSizeParametric
  } else if (optionsList$wantsWilcox && optionsList$onlyTest) {
    nameOfLocationParameter <- gettext("Hodges-Lehmann Estimate")
    nameOfEffectSize        <- gettext("Rank-Biserial Correlation")
  } else if (optionsList$wantsWilcox && (optionsList$wantsStudents || optionsList$wantsWelchs)) {
    nameOfLocationParameter <-  gettext("Location Parameter")
    nameOfEffectSize        <-  gettext("Effect Size")
  }

  if (optionsList$wantsStudents && optionsList$wantsWelchs)
    testInNote <- gettext("Student t-test and Welch t-test")
  else if (optionsList$wantsStudents)
    testInNote <- gettext("Student t-test")
  else if (optionsList$wantsWelchs)
    testInNote <- gettext("Welch t-test")

  ## add mean difference and standard error difference
  if (optionsList$wantsDifference) {
    ttest$addColumnInfo(name = "md", title = nameOfLocationParameter, type = "number")

    if (!(optionsList$wantsWilcox && optionsList$onlyTest))  # Only add SE Difference if not only MannWhitney is requested
      ttest$addColumnInfo(name = "sed", title = gettext("SE Difference"), type = "number")

    if (optionsList$wantsWilcox && (optionsList$wantsStudents || optionsList$wantsWelchs))
      ttest$addFootnote(gettextf("For the %s, location parameter is given by mean difference. For the Mann-Whitney test, location parameter is given by the Hodges-Lehmann estimate.", testInNote))
  }

  if (optionsList$wantsConfidenceMeanDiff) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceMeanDiff, nameOfLocationParameter)
    ttest$addColumnInfo(name = "lowerCIlocationParameter", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIlocationParameter", type = "number", title = gettext("Upper"), overtitle = title)
  }

  ## add Cohen's d
  if (optionsList$wantsEffect) {
    ttest$addColumnInfo(name = "d",            title = nameOfEffectSize,                      type = "number")
    ttest$addColumnInfo(name = "effectSizeSe", title = gettextf("SE %1$s", nameOfEffectSize), type = "number")

    if (optionsList$wantsWilcox) {
      wNote <- gettext("For the Mann-Whitney test, effect size is given by the rank biserial correlation.")

      twNote <- NULL
      if (optionsList$wantsStudents || optionsList$wantsWelchs)
        twNote <- gettextf("For the %1$s, effect size is given by %2$s.", testInNote, nameOfEffectSizeParametric)

      ttest$addFootnote(paste(twNote, wNote))
    }
  }

  if (optionsList$wantsConfidenceEffSize) {
    title <- gettextf("%1$s%% CI for %2$s", 100 * optionsList$percentConfidenceEffSize, nameOfEffectSize)
    ttest$addColumnInfo(name = "lowerCIeffectSize", type = "number", title = gettext("Lower"), overtitle = title)
    ttest$addColumnInfo(name = "upperCIeffectSize", type = "number", title = gettext("Upper"), overtitle = title)
  }

  jaspResults[["ttest"]] <- ttest

  if (ready)
    .ttestIndependentMainFill(jaspResults, ttest, dataset, options, testStat, optionsList)
}

.ttestIndependentNormalTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$normalityTest || !is.null(container[["ttestNormalTable"]]))
    return()
  container <- jaspResults[["AssumptionChecks"]]
  # Create table
  ttestNormalTable <- createJaspTable(title = gettext("Test of Normality (Shapiro-Wilk)"))
  ttestNormalTable$showSpecifiedColumnsOnly <- TRUE
  ttestNormalTable$position <- 2

  ttestNormalTable$addColumnInfo(name = "dep", type = "string", title = "", combine = TRUE)
  ttestNormalTable$addColumnInfo(name = "lev", type = "string", title = "")
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = gettext("W"))
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = gettext("p"))

  message <- gettext("Significant results suggest a deviation from normality.")
  ttestNormalTable$addFootnote(message)

  container[["ttestNormalTable"]] <- ttestNormalTable

  if (ready)
    .ttestIndependentNormalFill(jaspResults, ttestNormalTable, dataset, options)
}

.ttestIndependentEqVarTable <- function(jaspResults, dataset, options, ready, type){
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]

  if (!options$equalityOfVariancesTest || !is.null(container[["equalityVariance"]]))
    return()

  nameOfEqVarTest <- switch(options$equalityOfVariancesTestType,
                            "brownForsythe" = gettext("Brown-Forsythe"),
                            "levene" = gettext("Levene's"))

  # Create table
  equalityVariance <- createJaspTable(title = gettextf("Test of Equality of Variances (%1$s)", nameOfEqVarTest))
  equalityVariance$dependOn(c("equalityOfVariancesTestType"))
  equalityVariance$showSpecifiedColumnsOnly <- TRUE
  equalityVariance$position <- 3
  equalityVariance$addColumnInfo(name = "variable", type = "string",  title = "")
  equalityVariance$addColumnInfo(name = "fStat",    type = "number",  title = gettext("F"))
  equalityVariance$addColumnInfo(name = "dfOne",    type = "integer", title = gettextf("%s<sub>1</sub>", "df"))
  equalityVariance$addColumnInfo(name = "dfTwo",    type = "integer", title = gettextf("%s<sub>2</sub>", "df"))
  equalityVariance$addColumnInfo(name = "p",        type = "pvalue",  title = gettext("p"))

  container[["equalityVariance"]] <- equalityVariance

  if (ready)
    .ttestIndependentEqVarFill(jaspResults, equalityVariance, dataset, options)
}

.ttestIndependentMainFill <- function(jaspResults, table, dataset, options, testStat, optionsList) {

  mainTableData <- data.frame()
  mainTableResults <- createJaspState()
  jaspResults[["mainTableResults"]] <- mainTableResults

  if (options$effectSizeType == "cohen")
    effSize <- "cohen"
  else if (options$effectSizeType == "glass")
    effSize <- "glass"
  else if (options$effectSizeType == "hedges")
    effSize <- "hedges"

  levels <- levels(dataset[[ options$group ]])

  if (options$alternative == "greater" || options$alternative == "less") {
    directionNote <- ifelse(options$alternative == "greater", gettext("greater"), gettext("less"))
    table$addFootnote(gettextf("For all tests, the alternative hypothesis specifies that group %1$s is %2$s than group %3$s.",
                                                paste("<em>", levels[1], "</em>"), directionNote, paste("<em>", levels[2], "</em>")))
  }

  ## for each variable specified, run each test that the user wants
  for (variable in options$dependent) {
    errors <- .hasErrors(dataset,
                         message = 'short',
                         type = c('observations', 'variance', 'infinity'),
                         all.target = variable,
                         all.grouping = options$group,
                         observations.amount = '< 2')

    for (test in optionsList$whichTests) {

      row     <- list(v = variable, test = test, .isNewGroup = .ttestRowIsNewGroup(test, optionsList$whichTests))
      rowName <- paste(test, variable, sep = "-")

      errorMessage <- NULL
      if (identical(errors, FALSE)) {
        result <- try(ttestIndependentMainTableRow(variable, dataset, test, testStat, effSize, optionsList, options))

        if (!isTryError(result)) {
          row <- c(row, result[["row"]])
          if (result[["leveneViolated"]] && options$equalityOfVariancesTestType == "brownForsythe")
            table$addFootnote(gettext("Brown-Forsythe test is significant (p < .05), suggesting a violation of the equal variance assumption"), colNames = "p", rowNames = rowName)
          else if (result[["leveneViolated"]] && options$equalityOfVariancesTestType == "levene")
            table$addFootnote(gettext("Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption"), colNames = "p", rowNames = rowName)
        } else {
          errorMessage <- .extractErrorMessage(result)
        }

      } else {
        errorMessage <- errors$message
      }

      if (!is.null(errorMessage)) {
        row[[testStat]] <- NaN
        table$addFootnote(errorMessage, colNames = testStat, rowNames = rowName)
      }

      table$addRows(row, rowNames = rowName)
	# SHOW programmers
      mainTableData <- rbind(mainTableData,do.call(cbind,result[["row"]]))
      rownames(mainTableData[nrow(mainTableData),]) <- rowName
    }

    if (effSize == "glass") {
      ns  <- tapply(dataset[[variable]], dataset[[options$group]], function(x) length(na.omit(x)))
      sdMessage <- gettextf("Glass' delta uses the standard deviation of group %1$s of variable %2$s.", names(ns[2]), options$group)
      table$addFootnote(sdMessage)
    }
  }
    mainTableResults$object <- mainTableData
}

ttestIndependentMainTableRow <- function(variable, dataset, test, testStat, effSize, optionsList, options) {
  ciEffSize  <- optionsList$percentConfidenceEffSize
  ciMeanDiff <- optionsList$percentConfidenceMeanDiff
  f <- as.formula(paste(variable, "~",
                        options$group))

  variableData <- dataset[[ variable ]]
  groupingData <- dataset[[ options$group ]]

  sds <- tapply(variableData, groupingData, sd, na.rm = TRUE)
  ms  <- tapply(variableData, groupingData, mean, na.rm = TRUE)
  ns  <- tapply(variableData, groupingData, function(x) length(na.omit(x)))

  direction <- .ttestMainGetDirection(options$alternative)

  if (test == "Mann-Whitney") {
    r <- stats::wilcox.test(f, data = dataset,
                            alternative = direction,
                            conf.int = TRUE, conf.level = ciMeanDiff)
	# SHOW programmers changing "" to NA
    df   <- NA
    sed  <- NA
    stat <- as.numeric(r$statistic)
    m    <- as.numeric(r$estimate)
    d    <- abs(as.numeric(1-(2*stat)/(ns[1]*ns[2]))) * sign(m) # rankBis
    wSE <- sqrt((ns[1]*ns[2] * (ns[1]+ns[2] + 1))/12)
    rankBisSE <- sqrt(4 * 1/(ns[1]*ns[2])^2 * wSE^2)
    zRankBis  <- atanh(d)

    if(direction == "two.sided")
      confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize)/2)*rankBisSE),
                               tanh(zRankBis + qnorm((1+ciEffSize)/2)*rankBisSE)))
    else if (direction == "less")
      confIntEffSize <- sort(c(-Inf, tanh(zRankBis + qnorm(ciEffSize)*rankBisSE)))
    else if (direction == "greater")
      confIntEffSize <- sort(c(tanh(zRankBis + qnorm((1-ciEffSize))*rankBisSE), Inf))

    effectSizeSe <- tanh(rankBisSE)
  } else {
    r <- stats::t.test(f, data = dataset, alternative = direction,
                       var.equal = test != "Welch", conf.level = ciMeanDiff)

    df   <- as.numeric(r$parameter)
    m    <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
    stat <- as.numeric(r$statistic)

    num <-  (ns[1] - 1) * sds[1]^2 + (ns[2] - 1) * sds[2]^2
    sdPooled <- sqrt(num / (ns[1] + ns[2] - 2))
    if (test == "Welch")  # Use different SE when using Welch T test!
      sdPooled <- sqrt(((sds[1]^2) + (sds[2]^2)) / 2)

    d <- NA # SHOW programmers: Changed from ".", as that broke lapply functions.
    if (optionsList$wantsEffect) {
      # Sources are https://en.wikipedia.org/wiki/Effect_size for now.
      if (options$effectSizeType == "cohen")
        d <- as.numeric((ms[1] - ms[2]) / sdPooled)
      else if (options$effectSizeType == "glass")
        d <- as.numeric((ms[1] - ms[2]) / sds[2])
      # Should give feedback on which data is considered 2.
      else if (options$effectSizeType == "hedges") {
        a <- sum(ns) - 2
        logCorrection <- lgamma(a / 2) - (log(sqrt(a / 2)) + lgamma((a - 1) / 2))
        d <- as.numeric((ms[1] - ms[2]) / sdPooled) * exp(logCorrection) # less biased / corrected version
      }
    }
    sed <- (as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])) / stat

    #compute effect size SE
    j <- ifelse(effSize == "hedges", exp(logCorrection), 1)
    ni <- ifelse(effSize == "glass", ns[2], ns)

    effectSizeVar <- as.numeric(j)^2 * (sum(ns)/prod(ns) + (as.numeric(d)^2 / (2*sum(ni))))
    #Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges, J. P. T. Higgins and H. R. Rothstein (2009). Chapter 4, equation (4.20/4.24).
    effectSizeSe <- sqrt(effectSizeVar)

    confIntEffSize <- c(0,0)

    if (optionsList$wantsConfidenceEffSize){
      # From MBESS package by Ken Kelley, v4.6
      dfEffSize  <-  ifelse(effSize == "glass", ns[2] - 1, df)
      alphaLevel <- ifelse(direction == "two.sided", 1 - (ciEffSize + 1) / 2, 1 - ciEffSize)
      confIntEffSize <- .confidenceLimitsEffectSizes(ncp = d * sqrt((prod(ns)) / (sum(ns))),
                                                     df = dfEffSize, alpha.lower = alphaLevel,
                                                     alpha.upper = alphaLevel)[c(1, 3)]
      confIntEffSize <- unlist(confIntEffSize) * sqrt((sum(ns)) / (prod(ns)))

      if (direction == "greater")
        confIntEffSize[2] <- Inf
      else if (direction == "less")
        confIntEffSize[1] <- -Inf

      confIntEffSize <- sort(confIntEffSize)
    }
  }
  ## if the user doesn't want a Welch's t-test or Mann-Whitney,
  ## give a footnote indicating if the equality of variance
  ## assumption is met; seems like in this setting there is no
  ## sampling plan, thus the p-value is not defined. haha!
  leveneViolated <- FALSE
  if (!optionsList$wantsWelchs && !optionsList$wantsWilcox && optionsList$wantsStudents) {

    if (options$equalityOfVariancesTestType == "brownForsythe")
      center <- "median"
    else
      center <- "mean"

    levene <- car::leveneTest(variableData, groupingData, center)
    ## arbitrary cut-offs are arbitrary
    if (!is.na(levene[1, 3]) && levene[1, 3] < 0.05)
      leveneViolated <- TRUE
  }

  ## same for all t-tests
  p     <- as.numeric(r$p.value)
  ciLow <- r$conf.int[1]
  ciUp  <- r$conf.int[2]
  lowerCIeffectSize <- as.numeric(confIntEffSize[1])
  upperCIeffectSize <- as.numeric(confIntEffSize[2])

  # this will be the results object
  row <- list(df = df, p = p, md = m, d = d,
              lowerCIlocationParameter = ciLow, upperCIlocationParameter = ciUp,
              lowerCIeffectSize = lowerCIeffectSize, upperCIeffectSize = upperCIeffectSize,
              effectSizeSe = effectSizeSe, sed = sed)

  row[[testStat]] <- stat

  if (options$vovkSellke)
    row[["VovkSellkeMPR"]] <- VovkSellkeMPR(p)

  return(list(row = row, leveneViolated = leveneViolated))
}
# TODO Remove next line
##### Other Tables Fill ----
.ttestIndependentNormalFill <- function(jaspResults, table, dataset, options) {
  ## for a independent t-test, we need to check both group vectors for normality

  normTableData <- data.frame()
  normTableResults <- createJaspState()
  jaspResults[["normTableResults"]] <- normTableResults
  variables <- options$dependent
  factor    <- options$group
  levels    <- levels(dataset[[factor]])

  for (variable in variables) {
    ## there will be two levels, otherwise .hasErrors will quit
    for (level in levels) {

      row     <- list(dep = variable, lev = level, .isNewGroup = (level == levels[1]))
      rowName <- paste(variable, level, sep = "-")

      errors <- .hasErrors(dataset,
                           message = 'short',
                           type = c('observations', 'variance', 'infinity'),
                           all.target = variable,
                           observations.amount = c('< 3', '> 5000'),
                           all.grouping = factor,
                           all.groupingLevel = level)

      if (!identical(errors, FALSE)) {
        row[["W"]] <- NaN
        table$addFootnote(errors$message, colNames = "W", rowNames = rowName)
      } else {
        ## get the dependent variable at a certain factor level
        data <- na.omit(dataset[[variable]][dataset[[factor]] == level])
        r <- stats::shapiro.test(data)
        row[["W"]] <- as.numeric(r$statistic)
        row[["p"]] <- r$p.value
      }
      normTableData <- rbind(normTableData, data.frame(
        dep = variable,
        lev = level,
        W = row[["W"]],
        p = row[["p"]],
        stringsAsFactors = FALSE
      ))

      table$addRows(row, rowNames = rowName)
    }
  }
  normTableResults$object <- normTableData
}

.ttestIndependentEqVarFill <- function(jaspResults, table, dataset, options) {

  eqvarTableData <- data.frame()
  eqvarTableResults <- createJaspState()
  jaspResults[["eqvarTableResults"]] <- eqvarTableResults

  variables <- options$dependent
  groups    <- options$group

  levels <- levels(dataset[[ groups ]])

  for (variable in variables) {

    row <- list(variable = variable)

    errors <- .hasErrors(dataset,
                        message = 'short',
                        type = c('observations', 'variance', 'infinity'),
                        all.target = variable,
                        observations.amount = c('< 3'),
                        all.grouping = groups)

    errorMessage <- NULL
    if (identical(errors, FALSE)) {
      result <- try(.ttestIndependentEqVarRow(table, variable, groups, dataset, options))

      if (!isTryError(result))
        row <- c(row, result[["row"]])
      else
        errorMessage <- .extractErrorMessage(result)

    } else {
      errorMessage <- errors$message
    }

    # Fill the df with the data generated to capture the tests row-wise per variable
    eqvarTableData <- rbind(eqvarTableData,do.call(cbind,result[["row"]]))

    if (!is.null(errorMessage)) {
      row[["fStat"]] <- NaN
      table$addFootnote(errorMessage, colNames = "fStat", rowNames = variable)
    } else if (!result[["LeveneComputed"]])
      table$addFootnote(gettext("F-statistic could not be calculated"), colNames = "fStat", rowNames = variable)

    table$addRows(row, rowNames = variable)

  }
  eqvarTableResults$object <- eqvarTableData
}

.ttestIndependentEqVarRow <- function(table, variable, groups, dataset, options) {

  if (options$equalityOfVariancesTestType == "brownForsythe")
    center <- "median"
  else
    center <- "mean"

  levene <- car::leveneTest(dataset[[ variable ]], dataset[[ groups ]], center)


  fStat  <- levene[1, "F value"]
  dfOne <- levene[1, "Df"]
  dfTwo <- levene[2, "Df"]
  p  <- levene[1, "Pr(>F)"]

  row <- list(fStat = fStat, dfOne = dfOne, dfTwo = dfTwo, p = p)

  LeveneComputed <- TRUE
  if (is.na(levene[1, "F value"]))
    LeveneComputed <- FALSE

  return(list(row = row, LeveneComputed = LeveneComputed))
}

.ttestIndependentDescriptivesTable <- function(jaspResults, dataset, options, ready) {
  # Container
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (!options$descriptives || !is.null(container[["table"]]))
    return()
  # Create table
  ttestDescriptivesTable <- createJaspTable(title = gettext("Group Descriptives"), dependencies = "descriptives")
  ttestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  ttestDescriptivesTable$position <- 4
  ttestDescriptivesTable$addColumnInfo(name = "variable", type = "string",  title = "", combine = TRUE)
  ttestDescriptivesTable$addColumnInfo(name = "group",    type = "string",  title = gettext("Group"))
  ttestDescriptivesTable$addColumnInfo(name = "N",        type = "integer", title = gettext("N"))
  ttestDescriptivesTable$addColumnInfo(name = "mean",     type = "number",  title = gettext("Mean"))
  ttestDescriptivesTable$addColumnInfo(name = "sd",       type = "number",  title = gettext("SD"))
  ttestDescriptivesTable$addColumnInfo(name = "se",       type = "number",  title = gettext("SE"))
  ttestDescriptivesTable$addColumnInfo(name = "coefOfVariation",
                                                          type = "number",  title = gettext("Coefficient of variation"))
  if (options[["mannWhitneyU"]]) {
    ttestDescriptivesTable$addColumnInfo(name = "meanRank", type = "number",  title = gettext("Mean Rank"))
    ttestDescriptivesTable$addColumnInfo(name = "sumRank",  type = "number",  title = gettext("Sum Rank"))
  }
  ttestDescriptivesTable$dependOn("mannWhitneyU")

  container[["table"]] <- ttestDescriptivesTable

  if(ready)
    .ttestIndependentDescriptivesFill(ttestDescriptivesTable, dataset, options)
}

.ttestIndependentDescriptivesFill <- function(table, dataset, options) {
  variables <- options$dependent
  groups <- options$group
  levels <- base::levels(dataset[[ groups ]])
  groupingData <- dataset[[groups]]

  for (variable in variables) {

    for (level in levels) {

      row <- list(variable = variable, group = level, .isNewGroup = (level == levels[1]))

      variableData <- dataset[[variable]]
      groupData   <- variableData[groupingData == level]
      groupDataOm <- na.omit(groupData)

      dataRank <- rank((dataset[[variable]]), na.last = "keep")
      groupDataRank <- dataRank[groupingData == level]
      groupDataRankOm <- na.omit(groupDataRank)

      if (class(groupDataOm) != "factor") {

        n    <- length(groupDataOm)
        mean <- mean(groupDataOm)
        std  <- sd(groupDataOm)
        sem  <- std / sqrt(n)
        coefOfVariation <- std / mean

        meanRank <- mean(groupDataRankOm)
        sumRank <- sum(groupDataRankOm)

        row <- c(row, list(N = n, mean = mean, sd = std, se = sem,
                           coefOfVariation = coefOfVariation,
                           meanRank = meanRank,
                           sumRank = sumRank))

      } else {
        n   <- length(groupDataOm)
        row <- c(row, list(n = n))
      }

      table$addRows(row)
    }
  }
}

.ttestIndependentDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (is.null(container[["plots"]])) {
    subcontainer <- createJaspContainer(gettext("Descriptives Plots"), dependencies = c("descriptivesPlot", "descriptivesPlotCiLevel"))
    subcontainer$position <- 5
    container[["plots"]] <- subcontainer
  } else {
    subcontainer <- container[["plots"]]
  }

  for(variable in options$dependent) {
    if(!is.null(subcontainer[[variable]]))
      next
    descriptivesPlot <- createJaspPlot(title = variable, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[variable]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestIndependentDescriptivesPlotFill(dataset, options, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestIndependentDescriptivesPlotFill <- function(dataset, options, variable) {

  groups <- options$group
  errors <- .hasErrors(dataset,
                       message = 'short',
                       type = c('observations', 'variance', 'infinity'),
                       all.target = variable,
                       observations.amount = '< 2',
                       observations.grouping = groups)

  if (!isFALSE(errors))
    stop(errors$message)

  dataset <- na.omit(dataset[, c(groups, variable)])
  summaryStat <- .summarySE(
    dataset,
    measurevar    = variable,
    groupvars     = groups,
    conf.interval = options[["descriptivesPlotCiLevel"]],
    na.rm         = TRUE,
    .drop         = FALSE
  )

  colnames(summaryStat)[which(colnames(summaryStat) == variable)] <- "dependent"
  colnames(summaryStat)[which(colnames(summaryStat) == groups)]   <- "group"

  p <- jaspGraphs::descriptivesPlot(
    x                      = summaryStat[["group"]],
    y                      = summaryStat[["dependent"]],
    ciLower                = summaryStat[["ciLower"]],
    ciUpper                = summaryStat[["ciUpper"]],
    group                  = summaryStat[["group"]],
    noXLevelNames          = FALSE,
    yName                  = variable,
    xName                  = groups
  ) + jaspGraphs::themeJaspRaw(axis.title.cex = jaspGraphs::getGraphOption("axis.title.cex"))

  return(p)
}

.ttestIndependentDescriptivesRainCloudPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$raincloudPlot)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]

  if (is.null(container[["plotsRainCloud"]])) {
    subcontainer <- createJaspContainer(gettext("Raincloud Plots"), dependencies = c("raincloudPlot", "raincloudPlotHorizontal"))
    subcontainer$position <- 6
    container[["plotsRainCloud"]] <- subcontainer
  } else {
    subcontainer <- container[["plotsRainCloud"]]
  }
  horiz <- options$raincloudPlotHorizontal
  if(ready){
    groups <- options$group
    errors <- .ttestBayesianGetErrorsPerVariable(dataset, options, "independent")
    for(variable in options$dependent) {
      if(!is.null(subcontainer[[variable]]))
        next
      descriptivesPlotRainCloud <- createJaspPlot(title = variable, width = 480, height = 320)
      descriptivesPlotRainCloud$dependOn(optionContainsValue = list(variables = variable))
      subcontainer[[variable]] <- descriptivesPlotRainCloud
      if(!isFALSE(errors[[variable]])) {
        descriptivesPlotRainCloud$setError(errors[[variable]]$message)
        next
      }
      p <- try(.descriptivesPlotsRainCloudFill(dataset, variable, groups, variable, groups, addLines = FALSE, horiz, NULL))
      if(isTryError(p))
        descriptivesPlotRainCloud$setError(.extractErrorMessage(p))
      else
        descriptivesPlotRainCloud$plotObject <- p
    }
  }
  return()
}

.ttestIndependentDescriptivesBarPlot <- function(jaspResults, dataset, options, ready) {
  if (!options[["barPlot"]])
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (is.null(container[["barPlots"]])) {
    subcontainer <- createJaspContainer(gettext("Bar Plots"), dependencies = c("barPlot",
                                                                               "barPlotCiLevel",
                                                                               "barPlotErrorType",
                                                                               "barPlotYAxisFixedToZero"))
    subcontainer$position <- 7
    container[["barPlots"]] <- subcontainer
  } else {
    subcontainer <- container[["barPlots"]]
  }

  for (variable in options[["dependent"]]) {
    if (!is.null(subcontainer[[variable]]))
      next
    descriptivesBarPlot <- createJaspPlot(title = variable, width = 480, height = 320)
    descriptivesBarPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[variable]] <- descriptivesBarPlot
    if (ready) {
      p <- try(.ttestDescriptivesBarPlotFill(dataset, options, variable))
      if (isTryError(p))
        descriptivesBarPlot$setError(.extractErrorMessage(p))
      else
        descriptivesBarPlot$plotObject <- p
    }
  }
  return()
}


##### START AUTO-STAT ##########################################################
.ttestIndependentIntroText <- function(jaspResults, dataset, options, ready, type) {
    if (!is.null(jaspResults[["introText"]]))
        return()
    if (!options$roboReport)
        return()

    mtr <- jaspResults[["mainTableResults"]]$object # get data table
    jaspResults[["mainTableResults"]]$object <- mtr
    # Defining variables for text output
    alternative <- options$alternative # This works as a variable
    groups    <- options$group # This seems to refer to the name given the grouping var
    dependent <- options$dependent
    levels <- base::levels(dataset[[ groups ]])

    # Output branch alternative hypothesis
    if (alternative == "twoSided") {
        altHypoText <- "The report below is for a two-sided test, that is,
                 the alternative hypothesis does not state the direction of the
                 effect." }
    else if (alternative == "greater")
        altHypoText <- gettextf("The report below is testing whether the scores of
                 <b>%1$s</b> are greater than the scores of <b>%2$s</b>.",
                                levels[1],
                                levels[2] )
    else if (alternative == "less")
        altHypoText <- gettextf("The report below is testing whether the scores of
                 <b>%1$s</b> are less than the scores of <b>%2$s</b>.",
                                levels[1],
                                levels[2] )
    else altHypoText <- "No alternative hypothesis has been selected."

    # Create text
    introText <- createJaspHtml(text =
                 gettextf("This is an autostat report for an independent samples t-test.
                 Interest centers on the comparison of two groups (i.e., group =
                 <b>%1$s</b> versus group = <b>%2$s</b>) concerning their population
                 means for the dependent variable <b>%3$s</b>. The t-test assumes that
                 the <b>%3$s</b> data from each group are continuous and normally
                 distributed. %4$s",
                 levels[1], levels[2], dependent, altHypoText))
    introText$dependOn(c("dependent", "group", "alternative"))
    jaspResults[["introText"]] <- introText
}

.ttestIndependentSummaryText <- function(jaspResults, dataset, options, ready, type) {
  if (!options$roboReport)
    return()

  optionsList <- .ttestOptionsList(options, type)

  # Defining variables for text output
  mtr_obj <- jaspResults[["mainTableResults"]]$object # get data table
  mtr <- as.data.frame(mtr_obj, row.names = optionsList$whichTests)

  # EffectSize Type (adapted from .ttestIndependentMainTable)
  if (options$effectSizeType == "cohen")
      effSizeName <- "Cohen's d"
  else if (options$effectSizeType == "glass")
      effSizeName <- "Glass' delta"
  else if (options$effectSizeType == "hedges")
      effSizeName <- "Hedges' g"

  # uniformly naming the testStat "Statistic", copied from .ttestIndependentMainTable
  if (optionsList$wantsWilcox && optionsList$onlyTest) {
      names(mtr)[names(mtr) == 'U'] <- 'Statistic'
  } else if (optionsList$wantsWelchs && optionsList$onlyTest) {
      names(mtr)[names(mtr) == 't'] <- 'Statistic'
  } else if (optionsList$wantsStudents && optionsList$onlyTest) {
      names(mtr)[names(mtr) == 't'] <- 'Statistic'
  }

  # Summary Title
  summaryTitle <- createJaspHtml(
    text = gettextf("<h2>1. Executive Summary of the Default Analysis</h2>"))
  jaspResults[["summaryTitle"]] <- summaryTitle

  # Helper functions
  round_mtr <- function(x) lapply(x, round, digits = 3)

  format_test_result <- function(test_name, test_data, options) {
      effSizeName <- switch(options$effectSizeType,
                            "cohen" = "Cohen's d",
                            "glass" = "Glass' delta",
                            "hedges" = "Hedges' g")

      if (test_name == "Mann-Whitney") {
        result <- paste0("p = ", test_data$p,
                         ", U(", test_data$df, ") = ", test_data$Statistic)
      } else {result <- paste0("p = ", test_data$p,
                       ", t(", test_data$df, ") = ", test_data$Statistic)}
      return(result)
  }

  # Main function body
  mtr_rounded <- round_mtr(mtr)
  groups <- options$group
  dependent <- options$dependent
  levels <- base::levels(dataset[[groups]])

  # Convert the test selection to natural text form
  test_type <- c("Student t-test", "Welch t-test", "Mann-Whitney U test")
  user_selection <- c(options$student, options$welch, options$mannWhitneyU)
  selected_tests <- test_type[user_selection]
  tests_string <- if (length(selected_tests) == 1) {
      selected_tests
  } else if (length(selected_tests) == 2) {
      paste(selected_tests, collapse = " and ")
  } else {
      paste(paste(selected_tests[-length(selected_tests)], collapse = ", "),
            "and", selected_tests[length(selected_tests)])
  }

  # Effect Size Text
  summ_effectCi <- if (options$effectSizeCi) {
      sprintf(" and a 95%% confidence interval ranging from %s to %s",
              mtr_rounded$lowerCIeffectSize, mtr_rounded$upperCIeffectSize)
  } else ""

  summaryEffect <- if (options$effectSize) {
      sprintf("The corresponding value for %s equals %s, with a standard error of %s%s.",
              effSizeName, mtr_rounded$d, mtr_rounded$effectSizeSe, summ_effectCi)
  } else ""

  # Vovk-Sellke
  summaryVovkSellke <- if (options$vovkSellke) {
      vovkSellkeLevel <- if (mtr_rounded$VovkSellkeMPR > 10) {
          ", which is substantial [EJ APPROVAL FOR TEXT]"
      } else {
          ", which is not compelling and urges caution"
      }
      sprintf("The Vovk-Sellke maximum p-Ratio of %s indicates the maximum possible odds in favor of H1 over H0%s.",
              mtr_rounded$VovkSellkeMPR, vovkSellkeLevel)
  } else ""

  # Alternative Hypothesis
  summNullHypo <- switch(options$alternative,
                         "twoSided" = "no population difference between the groups",
                         "greater" = sprintf("the %s population being equal or greater than the %s population", levels[2], levels[1]),
                         "less" = sprintf("the %s population being equal or lesser than the %s population", levels[2], levels[1]))

  # Create individual summaries for each test
  test_summaries <- lapply(row.names(mtr), function(test) {
      test_data <- round_mtr(mtr[test,])
      significant <- test_data$p < 0.05
      significant_text <- if(significant) "" else "not "

      sprintf("For the %s test, the difference between %s is %sstatistically significant at the .05 level: %s
      %s
      We may %sreject the null-hypothesis of %s.
      Note that this does not mean that the data provide evidence %s the null hypothesis
      or provide evidence %s the alternative hypothesis for this test;
      it also does not mean that the null hypothesis is %s to hold.",
              test,
              paste(levels, collapse = " and "),
              significant_text,
              format_test_result(test, test_data, options),
              summaryEffect,
              significant_text,
              summNullHypo,
              if(significant) "against" else "for",
              if(significant) "for" else "against",
              if(significant) "unlikely" else "likely")
  })

  # Combine all summaries
  all_summaries <- paste(test_summaries, collapse = "\n\n")

  # Create the overall summary text
  text <- sprintf("The table above summarizes the outcome of the %s.
    The dependent variable is %s, and the grouping variable is %s with levels %s.
    The difference in the two sample means is %s, with a standard error of %s.

    %s

    These results also do not identify a likely range of values for effect size.
    In order to address these questions, a Bayesian analysis would be needed.

    %s",
                  tests_string,
                  dependent,
                  groups,
                  paste(levels, collapse = " and "),
                  mtr_rounded$md,
                  mtr_rounded$sed,
                  all_summaries,
                  summaryVovkSellke
                  )

  summaryText <- createJaspHtml(
      text <- gettextf("%1$s", text)
  )
  summaryText$dependOn(c("student", "welch", "mannWhitneyU", "group",
                         "dependent", "roboReport", "vovkSellke",
                         "effectSize", "effectSizeCI"))

  jaspResults[["summaryText"]] <- summaryText
}

.ttestDescriptivesText <- function(jaspResults, dataset, options, ready, type) {
    if (!options$roboReport || !options$descriptives)
        return()

    # Variables
    groups    <- options$group
    levels <- base::levels(dataset[[ groups ]])
    dependent <- options$dependent

    # Descriptives Title
    descripTitle <- createJaspHtml(
        text = gettextf("<h2>2. Descriptives</h2>"))
    jaspResults[["descripTitle"]] <- descripTitle

    # Text Raincloud Plot
    if (options$raincloudPlot)
        descripRainPlot <- ", followed by a raincloud plot that shows the individual observations"
    else descripRainPlot <- ""

    # Final output text
    descriptivesText <- createJaspHtml(
        text = gettextf("
            The table below summarizes the observed data for each group separately %1$s.<br>
            INSERT TABLE IN HERE, LIKELY WITH JASP CONTAINER<br>
            As can be seen from the table, group = <b>%1$s</b> contains 21 observations
            and has a mean <b>%3$s</b> of 51.4762 with a standard deviation of 11.0074;
            group = <b>%2$s</b> contains 23 observations and has a mean <b>%3$s</b> of 41.5217
            with a standard deviation of 17.1487. The observed mean <b>%3$s</b> in group = <b>%1$s</b>
            is higher than the observed mean <b>%3$s</b> in group = <b>%2$s</b>.
            This difference is [fork: is not] statistically significant at the .05 level:
            p=.0286, t(42) = -2.2666. We may reject [fork: may not reject] the null-hypothesis
            of no population difference between the groups. [NB. this needs to be adjusted for
            a one-sided test] Note that this does not mean that the data provide evidence
            against [fork: for] the null hypothesis or provide evidence for [fork: against]
            the alternative hypothesis; it also does not mean that the null hypothesis is
            unlikely [fork: likely] to hold. These results also do not identify a likely
            range of values for effect size. In order to address these questions a Bayesian
            analysis would be needed. The Vovk-Sellke maximum p-Ratio of 3.6162 indicates
            the maximum possible odds in favor of H1 over H0, which is not compelling and
            urges caution. [only include for odds lower than 10] [Note to Arne: these last
            sentences would clearly be part of a verbose report] [Note to Arne: we could
            also include a mention of whether the assumptions appear violated, and what
            a nonparametric test shows]",
            descripRainPlot,
            levels[1], levels[2], dependent))

    jaspResults[["descriptivesText"]] <- descriptivesText
}

.ttestAssumptionsText <- function(jaspResults, dataset, options, ready, type) {

  if (!options$roboReport || !options$textAssumptions)
      return()
  if (!options$normalityTest && !options$equalityOfVariancesTest)
      return()
  optionsList <- .ttestOptionsList(options, type)

  # Does not contain anything currently
  norm_obj <- jaspResults[["normTableResults"]]$object
  norm <- as.data.frame(norm_obj)

  eqvar_obj <- jaspResults[["eqvarTableResults"]]$object
  eqvar <- as.data.frame(eqvar_obj, row.names = options$dependent)
  eqvar <- round(eqvar, 3)
  # copied from above to use as conditional variable
  nameOfEqVarTest <- switch(options$equalityOfVariancesTestType,
                            "brownForsythe" = gettext("Brown-Forsythe"),
                            "levene" = gettext("Levene's"))
  nameOfEqVarTest <- ifelse(options$equalityOfVariancesTestType == "brownForsythe",
                            "brownForsythe",
                            "levene")
  eqvar_sig <- ifelse(eqvar$p > 0.05, "not", "")
  eqvar_rej <- ifelse(eqvar$p > 0.05, "reject", "retain")

  groups    <- options$group

  levels <- base::levels(dataset[[ groups ]])
  # norm_sig <- ifelse(norm_df["p"] > 0.05, "not", "")

  if (options$normalityTest) {
    # Create individual summaries for each test
    norm_summaries <- apply(norm, 1, function(row) {
      significant <- as.numeric(row["p"]) < 0.05
      sig_text <- if(significant) "" else "not "
      norm_sig <- if(significant) "" else "not "
      norm_rej <- if(significant) "reject" else "retain"

      sprintf("For group = <b>%s</b>, the Shapiro-Wilk test for normality is %s
      statistically significant at the .05 level (i.e., p = %s),
      and hence we %s the hypothesis that the data
      for group = <b>%s</b> are normally distributed.",
              row["lev"],
              norm_sig,
              round(as.numeric(row["p"]), 3),
              if(significant) "must reject" else "can retain",
              row["lev"])
    })
    all_summaries <- paste(norm_summaries, collapse = "\n")
    normalityText <- gettextf("%1$s [Note to Arne: in high-verbose
      level, we ought to add the reference to Shapiro-Wilk] Note that when the
      Shapiro-Wilk test is statistically nonsignificant this does not mean that
      the assumption of normality is met, or that the data support that
      assertion. Likewise, when the Shapiro-Wilk test is statistically
      significant this does not mean that the data provide evidence for
      the assertion that the data are not normally distributed. In order
      to address these questions a Bayesian analysis would be needed.<br>",
                              all_summaries)
  } else (normalityText <- "")

  if (options$equalityOfVariancesTest && (nameOfEqVarTest == "brownForsythe"))
    equalVarText <- gettextf("The Brown-Forsythe test for equality of variances is %1$s
      statistically significant at the .05 level: F(%2$s,%3$s) = %4$s, p = %5$s.
      Hence we can %6$s the null hypothesis that the variances
      in both groups are equal. Note that when the Brown-Forsythe test is
      statistically nonsignificant, this does not mean that the assumption
      of equal variance is met, or that the data support that assertion.
      Likewise, when the Brown-Forsythe test is statistically significant,
      this does not mean that the data provide evidence for the assertion
      that groups have different variances. In order to address these
      questions a Bayesian analysis would be needed.",
      eqvar_sig, eqvar$dfOne, eqvar$dfTwo, eqvar$fStat, eqvar$p, eqvar_rej)
  # else (equalVarText <- "") # TODO: Replace this with Levene below
  else if (options$equalityOfVariancesTest && (nameOfEqVarTest == "levene"))
    equalVarText <- gettextf("The Levene's test for equality of variances is %1$s
      statistically significant at the .05 level: F(%2$s,%3$s) = %4$s, p = %5$s.
      Hence we can %6$s the null hypothesis that the variances
      in both groups are equal. Note that when Levene's test is
      statistically nonsignificant this does not mean that the assumption
      of equal variance is met, or that the data support that assertion.
      Likewise, when the Levene's test is statistically significant
      this does not mean that the data provide evidence for the assertion
      that groups have different variances. In order to address these
      questions a Bayesian analysis would be needed.",
      eqvar_sig, eqvar$dfOne, eqvar$dfTwo, eqvar$fStat, eqvar$p, eqvar_rej)
  else (equalVarText <- "")

  assumptionsText <- createJaspHtml(
    text = gettextf("<h2>3. Assumption Checks</h2>
      %1$s
      INSERT TABLE IN HERE, LIKELY WITH JASP CONTAINER<br>
      %2$s",
      normalityText, equalVarText))
  assumptionsText$dependOn(c("dependent", "group", "normalityTest",
                             "equalityOfVariancesTest", "textAssumptions"))
  jaspResults[["assumptionsText"]] <- assumptionsText
}

.ttestParametersText <- function(jaspResults, dataset, options, ready, type) {
    if (!is.null(jaspResults[["parametersText"]]))
        return()
    if (!options$roboReport)
        return()

    groups    <- options$group
    levels <- base::levels(dataset[[ groups ]])


    # EffectSize Type (adapted from .ttestIndependentMainTable)
    if (options$effectSizeType == "cohen")
        effSizeName <- "Cohen's d"
    else if (options$effectSizeType == "glass")
        effSizeName <- "Glass' delta"
    else if (options$effectSizeType == "hedges")
        effSizeName <- "Hedges' g"

    parametersText <- createJaspHtml(
        text = gettextf("<h2>4. Parameter Estimation: How Strong is the Effect?</h2>
        As is apparent from the T-test table and the descriptive information,
        the mean drp is observed to be higher for group=<b>%1$s</b> than for
        group=<b>%2$s</b>. The location parameter equals the difference in the
        two sample means (i.e., 9.9545), with a standard error of 4.3919.
        The corresponding value for {effSizeName} equals -0.6841, with a standard
        error of 0.3182 and a 95%% confidence  interval ranging from -1.2895 to
        -0.0710. According to Cohen's classification scheme, the value of
        -0.6841 corresponds to an observed effect that is 'medium to large'.
        [note to Arne: we need to add a reference on this]<br>
        The Brown-Forsythe test for equality of variances was not [fork: omit 'not']
        significant at the .05 level, but we nevertheless [fork: and this is why we also]
        report the results from the Welch test, which assumes that the variances in the
        two groups are unequal. The location parameter in the Welch test equals
        the difference in the two sample means and the associated standard error
        is 4.3076. The corresponding value for {effSizeName} equals -0.6908, with a
        standard error of 0.3185 and a 95%% confidence interval ranging from -1.2981
        to -0.0750. According to Cohen's classification scheme, the value of
        -0.6908 corresponds to an observed effect that is 'medium to large'.<br>
        The Shapiro-Wilk test for normality was not [fork: omit 'not'] statistically
        significant at the .05 level, but we nevertheless [fork: and this is why
        we also] report the result from the Mann-Whitney test, which is based
        only on the ranks of the observations; therefore, the Mann-Whitney test
        is relatively robust. The Mann-Witney location parameter (i.e., the
        Hodges-Lehmann estimate) equals -10.0001. The Mann-Whitney effect size
        measure is the the rank biserial correlation; here it equals -0.4410,
        with a standard error of 0.1744 and a 95%% confidence interval that
        ranges from -0.6745 to -0.1274.<br>
        For all estimates: the above confidence intervals do not identify a likely
        range of values for effect size. In order to obtain this information a
        Bayesian analysis would be needed (e.g., Morey et al., 2016;
        van den Bergh, 2021).", levels[1], levels[2]))

    jaspResults[["parametersText"]] <- parametersText
}

.ttestHypothesisText <- function(jaspResults, dataset, options, ready, type) {
  if (!options$roboReport)
    return()
  hypothesisTitle <- createJaspHtml("<h2>5. Hypothesis Testing: Is The Effect Absent?</h2>")
  hypothesisTitle$dependOn(c("student", "welch", "mannWhitneyU", "group", "dependent", "roboReport"))

  optionsList <- .ttestOptionsList(options, type)
  groups    <- options$group
  levels <- base::levels(dataset[[ groups ]])

  mtr_obj <- jaspResults[["mainTableResults"]]$object # get data table
  mtr <- as.data.frame(mtr_obj, row.names = optionsList$whichTests)

  mtr_rounded <- lapply(mtr, round, digits = 3)
  significant <- mtr$p < 0.05
  signif_text <- if(significant) "" else "not "

  test_type <- c("standard t-", "Welch t-", "Mann-Whitney U")

  jaspResults[["hypothesisTitle"]] <- hypothesisTitle

  if (optionsList$wantsStudents) {
    hypothesisStudent <- createJaspHtml(
      text = gettextf("
        For the %7$stest, the group difference is %3$s
        statistically significant at the .05 level: p=%4$s, t(%5$s) = %6$s.
        We may %3$s reject the null-hypothesis of no population
        difference between the groups. [NB. this needs to be adjusted for a
        one-sided test] <br>",
                      levels[1], levels[2],
                      signif_text,  mtr_rounded["p"],
                      mtr_rounded["df"],
                      mtr_rounded["t"], "standard t-"))

    jaspResults[["hypothesisStudent"]] <- hypothesisStudent
  }

  if (optionsList$wantsWilcox) {
    hypothesisWilcox <- createJaspHtml(
      text = gettextf("
        For the %7$stest, the group difference is %3$s
        statistically significant at the .05 level: p=%4$s, t(%5$s) = %6$s.
        We may %3$s reject the null-hypothesis of no population
        difference between the groups. [NB. this needs to be adjusted for a
        one-sided test] The Vovk-Sellke maximum p-Ratio of 3.6162 indicates the
        maximum possible odds in favor of H1 over H0, which is not compelling and
        urges caution. [only include for odds lower than 10]<br>",
          levels[1], levels[2],
          signif_text,  mtr_rounded["p"],
          mtr_rounded["df"],
          mtr_rounded["t"], "Mann-Whitney U"))

    jaspResults[["hypothesisText"]] <- hypothesisWilcox
  }

  if (optionsList$wantsWelchs) {
    hypothesisWelch <- createJaspHtml(
      text = gettextf("
        For the %7$stest, the group difference is %3$s
        statistically significant at the .05 level: p=%4$s, t(%5$s) = %6$s.
        We may %3$s reject the null-hypothesis of no population
        difference between the groups. [NB. this needs to be adjusted for a
        one-sided test] The Vovk-Sellke maximum p-Ratio of 3.6162 indicates the
        maximum possible odds in favor of H1 over H0, which is not compelling and
        urges caution. [only include for odds lower than 10]<br>",
          levels[1], levels[2],
          signif_text,  mtr_rounded["p"],
          mtr_rounded["df"],
          mtr_rounded["t"], "Welch t-"))

    jaspResults[["hypothesisWelch"]] <- hypothesisWelch
  }

  hypothesisPval <- createJaspHtml(
    text = gettextf("The p-value does not quantify evidence for the null
    hypothesis versus the alternative hypothesis; the p-value also cannot be
    taken to mean that the null hypothesis is either likely or unlikely to
    hold, or that the data are more or less likely to occur under the null
    hypothesis than under the alternative hypothesis. In order to obtain this
    information a Bayesian analysis would be needed."))

  jaspResults[["hypothesisPval"]] <- hypothesisPval
}

.ttestReferences <- function(jaspResults, dataset, options, ready, type) {
  if (!options$roboReport)
    return()

  references <- createJaspHtml(
    text = gettextf("<h2>6. Further Information</h2>
    <h5>Mann-Whitney Non-Parametric Test / Wilcoxon Rank Sum</h5>
    Mann, H. B., & Whitney, D. R. (1947). On a test of whether one of two random variables is stochastically larger than the other. Annals of Mathematical Statistics, 18, 50–60.

    <h5>Shapiro-Wilk Normality Test</h5>
    Shapiro, S.S. and Wilk, M.B. (1965) An Analysis of Variance Test for Normality (Complete Samples). Biometrika, 52, 591-611. https://doi.org/10.1093/biomet/52.3-4.591

    <h5>Levene's test</h5>
    Levene, H. (1960) Robust Tests for Equality of Variances. In: Olkin, I., Ed., Contributions to Probability and Statistics, Stanford University Press, Palo Alto, 278-292.

    <h5>Raincloud Plot</h5>
    Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., & Kievit, R. A. (2019). Raincloud plots: a multi-platform tool for robust data visualization. Wellcome open research, 4, 63. https://doi.org/10.12688/wellcomeopenres.15191.1


    <h5>Student's t-test</h5>
    'Student' Gosset, W.G. (1908). The probable error of a mean. Biometrika. 6 (1): 1–25. doi:10.1093/biomet/6.1.1. hdl:10338.dmlcz/143545.

    <h5>Welch's t-test</h5>
    Welch, B. L. (1947). The generalization of 'Student's' problem when several different population variances are involved. Biometrika. 34 (1–2): 28–35. doi:10.1093/biomet/34.1-2.28"
                    )
  )
  jaspResults[["references"]] <- references
}
##### END AUTO-STAT ____________________________________________________________
