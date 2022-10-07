#------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------- PATIENT RISK PREDICTION ------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#
# PATIENTRISK() FUNCTION: predicts patient risk based on lab test results
# ultimately used to assign a risk level to each item, so that when selecting what to order, items that serve at-risk patients are favored
# Note: when generating patient risk data, real lab tests and results were used, but patient IDs and meidications were randomly assigned, because we 
# did not find any clear data linking a patient to a lab test result
patientRisk <- function() {
  library(RMySQL)
  con <- dbConnect(MySQL(), user="g1100792", password="group12", dbname="g1100792", host = "mydb.ics.purdue.edu")
  on.exit(dbDisconnect(con))
  
  # get all of the patients that have had lab tests, in addition to a list of ALL distinct lab tests that the hospital does
  patients <- dbGetQuery(con, paste('SELECT DISTINCT Patient_Id FROM Lab_Tests_Results'))
  labtests <- dbGetQuery(con, paste('SELECT DISTINCT Lab_ID FROM Lab_Tests_Results'))
  test_num <- length(labtests[,1])
  
  # get the first group of lab test results so that the others can be joined/combined in the loop below
  riskData <- dbGetQuery(con, paste('SELECT DISTINCT L.Patient_Id, y.Results FROM Lab_Tests_Results as L LEFT OUTER JOIN (SELECT Patient_Id, Results FROM Lab_Tests_Results WHERE Lab_ID = 1 GROUP BY patient_ID) y on y.Patient_ID = L.Patient_ID'))
  
  # combine all lab test results into a single data frame to be fed into the neural network
  for (i in 2:test_num) {
    test <- dbGetQuery(con, paste('SELECT DISTINCT L.Patient_Id, y.Results FROM Lab_Tests_Results as L LEFT OUTER JOIN (SELECT Patient_Id, Results FROM Lab_Tests_Results WHERE Lab_ID =', i, ' GROUP BY patient_ID) y on y.Patient_ID = L.Patient_ID'))
    riskData <- cbind(riskData, test[,2])
  }
  
  riskData[is.na(riskData)] <- 0
  
  
  # create training and testing data to train model; use some of the previous data to retrain model
  ind <- sample.int(dim(riskData)[1], round(0.50*dim(riskData)[1]), replace=FALSE)
  trainD <- riskData[,-1]
  trainD <- trainD[ind,]
  testD <- riskData[-1]
  
  # send to neural network
  require(nnet)
  nnet.fit <- nnet(trainD[,length(trainD)] ~ ., data=trainD, size=6, linout = TRUE, maxit = 1000)
  nnet.predict <- ceiling(predict(nnet.fit, testD))
  nnet.predict[which(nnet.predict > 3)] <- 3
  
  # # update patient risk in the database
  # for (i in 1:length(risk$Risk)) {
  #   dbSendQuery(con, paste('UPDATE Patients SET Risk =', risk$Risk[i], 'WHERE Patient_ID =', risk$Patient_Id[i]))
  # }
  
  # use patient risk to give items an associated "risk"
  # assuming patient and item risk is 1 (low risk), unless lab test and prescription results indicate otherwise
  medRisk <- dbGetQuery(con, paste('SELECT Item_ID_Bottle as Item_ID, AVG(Risk) FROM Bottle AS B INNER JOIN Patients AS P WHERE P.Patient_ID = B.Patient_ID_Bottle GROUP BY Item_ID_Bottle'))
  
  # disconnect from database
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons)
    dbDisconnect(con)
  
  # ensure that all risk levels are categorized as 1, 2, 3
  medRisk$`AVG(Risk)` <- round(medRisk$`AVG(Risk)`)
  
  # assign new risk values to the global data frame based on patient risk prediction model
  for (i in 1:length(initial$Item_ID)) {
    for (j in 1:length(medRisk$`AVG(Risk)`)) {
      if (initial$Item_ID[i] == medRisk$Item_ID[j]) {
        initial$Risk[i] <- medRisk$`AVG(Risk)`[j]
      }
    }
  }
  
  
  # disconnect from database
  all_cons <- dbListConnections(MySQL())
  for (con in all_cons)
    dbDisconnect(con)
}


# https://heuristically.wordpress.com/2011/11/17/using-neural-network-for-regression/
#-----------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------- NEED PREDICTION ---------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#

# NEEDPREDICT() FUNCTION: predicting "need" of each item for next ordering period
# takes into account past "use" of each item during each ordering period to determine how much is likely to be used the following period
needpredict <- function(allData) {
  # CREATE NEURAL NETWORK
  ans <- allData[,length(allData)]   # most recent order - used to test accuracy of model in while loop
  
  # train the model without most recent order period
  trainD <- allData[, c(-1, -length(allData))]
  
  # get prediction without including oldest order period, but include most recent ordering period
  testD <- allData[,c(-1, -2)]
  
  # scale data based on max values
  trainMax <- max(trainD)
  trainD <- trainD / trainMax
  testMax <- max(testD)
  testD <- testD / testMax
  
  # fit neural network
  require(nnet)
  nnet.fit <- nnet(trainD[,length(trainD)] ~ ., data=trainD, size=6, linout = TRUE, maxit = 1000)
  # predict and make sure to unscale value
  nnet.predict <- ceiling((predict(nnet.fit) * trainMax))
  nnet.predict
  
  # recalculate MSE
  MSE <- mean((nnet.predict - ans)^2)
  MSE
  
  # trying to keep the MSE small so that the prediction is as accurate as possible
  # enters loop if MSE should be smaller!
  while ((MSE / mean(allData[,length(allData)])) > 2) {
    # fit neural network
    nnet.fit <- nnet(trainD[,length(trainD)] ~ ., data=trainD, size=6, linout = TRUE, maxit = 1000)
    # predict and make sure to unscale value
    nnet.predict <- ceiling((predict(nnet.fit) * trainMax))
    nnet.predict
    
    # recalculate MSE
    MSE <- mean((nnet.predict - ans)^2)
    MSE
  }
  
  # predict need for next ordering period using past data
  need <- ceiling(predict(nnet.fit, testD, type = "raw") * testMax)
  
  # FOR COMPARING TO TEST DATA
  results <- data.frame("predicted" = nnet.predict, "actual" = ans)
  plot(ans, nnet.predict, main=paste("nnet Predictions vs Actual"), sub=paste("MSE: ", MSE), xlab="Actual")
  abline(lm(nnet.predict~allData[,length(allData)]), col="red") # regression line (y~x)
  
  return(nnet.predict)
}


#------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------- ADJUST FOR CURRENT STOCK ---------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#


# EXPIREDADJUST() FUNCTION: Adjust the "need" based on the current stock of the hospital and any expiring products
# general idea: if the hospital needs 8 bottles of Advil, has 4 in stock, and 2 are expiring, their "ideal order" would be 6 bottles
# to account for what they have, and what they will lose to expiring
expiredAdjust <- function(need) {
  # medID is a global variable
  # making a dataframe that contains the need, current stock, and expiring stock of each item so that the adjusted amount can be easily calculated
  needAdjust <- cbind(medID, need)
  needAdjust$expired <- rep(0, length(needAdjust[,1]))
  needAdjust$current <- rep(0, length(needAdjust[,1]))
  
  # match expiration dates and current stocks to medicine IDs
  for (i in 1:length(needAdjust$Item_ID)) {
    for (j in 1:length(expired$Item_ID)) {
      if (expired$Item_ID[j] == needAdjust$Item_ID[i]) {
        needAdjust$expired[i] <- expired$expireCount[j]
      }
    }
  }
  
  for (i in 1:length(needAdjust$Item_ID)) {
    for (j in 1:length(currentStock$Item_ID)) {
      if (currentStock$Item_ID[j] == needAdjust$Item_ID[i]) {
        needAdjust$current[i] <- currentStock$currentStock[j]
      }
    }
  }
  
  # actually adjust "need" based on current and expiring stock
  for (i in 1:length(needAdjust$Item_ID)) {
    # adjsut for current and expired stock
    needAdjust$need[i] <- needAdjust$need[i] + needAdjust$expired[i] - needAdjust$current[i]
  }
  
  # resave new need and return
  need <- needAdjust$need
  need <- abs(floor(need)) # makes sure that the "need" is a positive integer
  
  return(need)
}


# Git Hub code for reference/basis
# https://github.com/jeffheaton/aifh/blob/master/vol1/r-examples/ch9/knapsack.R
# simulated annealing
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html
#------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------ SIMULATED ANNEALING FUCNTIONS -----------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#


# SCORE() FUNCTION: function to be optimized - checking that we are staying within cost constraint
score <- function(contents) {
  result <- maxCost - sum(as.numeric(cost)*as.numeric(contents))
  if(result < 0) {
    result <- .Machine$double.maxx
  }
  result
}


# NEIGHBOR() function: called by "sann"; adds or removes items from total order
# trying to find local max to add the most "optimal" items to the order
# favoring items that serve the most patients (high need), cost the least (low cost), and serve at-risk patients (high risk)
moveNeighbor <- function(contents) {
  # always set "target" to first index
  target <- 1
  # loop that choose points to add/remove until the total order cost is less than the budget
  repeat {
    # randomly either add or remove; note that we remove more often (80% of the time)
    if(runif(1,0,1) > 0.30)  {
      # remove an item
      sampler <- 1:length(cost)
      target <- sample(sampler, 1)
      # remove different amount based on current amount of item in the order; 20% of need if none added yet, 10% otherwise
      # (adding less if it is already on the order)
      if((contents[target] - initial$Need[target]*0.20) > 0)  {
        contents[target] <- contents[target] - floor(initial$Need[target]*0.20)
      } else if ((contents[target] - initial$Need[target]*0.10) > 0) {
        contents[target] <- contents[target] - floor(initial$Need[target]*0.10)
      } 
    }  else  {
      # add an item 
      sampler <- 1:length(cost)
      target <- sample(sampler, 1)
      # favor items that have a higher need:cost ratio
      # items that are most "needed" by hospital, and items that are associated with high risk patients
      if((initial$Ratio[target] >= 0.25 * mean(initial$Ratio * contents)) || (initial$Need[target] >= 0.90*max(initial$Need))) {
        if (contents[target] < 1 && initial$Risk > 2) {
          # add 25% of the need to the order if none of that item are on the order yet AND if it is a high risk
          contents[target] <- contents[target] + floor(initial$Need[target]*0.40)
          if (contents[target] >= initial$Need[target]) {
            # make sure that there is never more purchased than needed
            contents[target] <- initial$Need[target]
          }
        } else if (contents[target] < 1) {
          # make sure that there is never more purchased than needed
          contents[target] <- contents[target] + floor(initial$Need[target]*0.40)
          if (contents[target] >= initial$Need[target]) {
            # make sure that there is never more purchased than needed
            contents[target] <- initial$Need[target]
          }
        } else {
          # if that item is already being ordered, add a little less to the order
          contents[target] <- contents[target] + floor(initial$Need[target]*0.20)
          # make sure that there is never more purchased than needed
          if (contents[target] >= initial$Need[target]) {
            contents[target] <- initial$Need[target]
          }
        }
      }
    }
    
    # we are done when the current cost of the order is below max cost
    # should never end an iteration spending more than the budget
    if((sum(initial$Cost*contents) < maxCost)) {
      break
    }
  }
  contents
}

# SIMULATEDANNEALINGFUNC() - prepares data for simulated annealing and called sann function
simulatedAnnealingFunc <- function(initial) {
  finalCosts <- 0
  result <- optim(rep(0,length(initial$Cost)), score, moveNeighbor, method = "SANN", control = list(maxit = 10000, temp = 10, trace = TRUE) )
  
  # get the result quantities
  qty <- result$par
  
  # save results (total cost, and also total needs met)
  finalCosts <- sum(as.numeric(initial$Cost)*qty)
  
  # save final order to a new data frame
  final_order <- data.frame("Cost" = as.numeric(initial$Cost), "Need" = as.numeric(initial$Need), 
                            "Ratio" = initial$Ratio, "Quantity" = as.numeric(qty))
  final_order$Order.Costs <- as.numeric(final_order$Cost) * as.numeric(final_order$Quantity)
  
  return (final_order)
}


#------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------- GREEDY FUNCTION ---------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#


# GREEDYFUNC() - takes output from simulated annealing & adds items using "greedy" framework until reaching budget
greedyFunc <- function(final_order) {
  # order by ratio so that item with the best need-cost ratio is first
  # filling order up to the maximum possible budget (in case simulated annealing does not fill order to max budget)
  # only taking into account need/cost ratio since simulated annealing should have accounted for other factors for the majority of the order
  final_order <- final_order[order(final_order$Ratio, decreasing = TRUE),]
  need_index <- 1
  
  # how must the order costs right now
  finalCosts <- sum(as.numeric(final_order$Cost)*as.numeric(final_order$Quantity))
  
  # continue to add to order until budget is met
  while (finalCosts < maxCost) {
    if (final_order$Need[need_index] > final_order$Quantity[need_index]) {
      # add 25% of the need to the order; make sure it does not go over the "need" and waste money
      final_order$Quantity[need_index] <-  final_order$Quantity[need_index] + floor(final_order$Need[need_index]*0.10)
      if (final_order$Quantity[need_index] > final_order$Need[need_index]) {
        final_order$Quantity[need_index] <- final_order$Need[need_index]
      }
    }
    
    # update total costs calculations
    finalCosts <- sum(as.numeric(final_order$Cost)*as.numeric(final_order$Quantity))
    
    # update need index
    need_index <- need_index + 1
    if (need_index > length(final_order$Cost)) {
      need_index <- 1
    }
  }
  
  # if the cost is too great, decrease quantity until no longer over budget
  while (finalCosts > maxCost) {
    if (final_order$Quantity[need_index] > 1) {
      final_order$Quantity[need_index] <- final_order$Quantity[need_index] - 1
    }
    if (final_order$Quantity[need_index] < 2) {
      need_index <- need_index + 1
    }
    final_order$Order.Costs <- as.numeric(final_order$Cost) * as.numeric(final_order$Quantity)
    finalCosts <- sum(as.numeric(final_order$Cost)*as.numeric(final_order$Quantity))
  }
  
  return (final_order)
}


#------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------ DATABASE CONNECTION ---------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#

# connect to database
library(RMySQL)
con <- dbConnect(MySQL(), user="g1100792", password="group12", dbname="g1100792", host = "mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

# gets distint order periods so far
periods <- dbGetQuery(con, 'SELECT DISTINCT Order_Date_Bottle from Bottle')

# number of order periods (number of sets of training data)
num_periods <- length(periods[,1])

# determine next order!
lastOrder <- periods[length(periods[,1]),]
tempDate <- seq(as.Date(lastOrder), length = 3, by = "+2 months")
orderDate <- tempDate[2]
nextOrder <- tempDate[3]

# declare data frame
bottles <- dbGetQuery(con, 'SELECT DISTINCT Item_ID_Bottle as Item_ID FROM Bottle')
medID <- bottles

# loop through queries to create the dataframe of "past order data" to send to the prediction algorithm
for (i in 1:num_periods) {
  bottles <- cbind(bottles, dbGetQuery(con, paste('SELECT SUM(CASE WHEN B.Order_Date_Bottle = \'', periods[i,], 
                                                  '\' THEN 1 ELSE 0 END) as Period FROM Medicine_Stock AS M 
                                                  LEFT OUTER JOIN Bottle AS B ON B.Item_ID_Bottle = M.Item_ID GROUP BY M.Item_ID')))
}

# query the cost of all items
cost <- dbGetQuery(con, 'SELECT M.Price FROM Medicine_Stock AS M LEFT OUTER JOIN Bottle AS B ON B.Item_ID_Bottle = M.Item_ID GROUP BY M.Item_ID')
cost <- as.numeric(cost[,1])

# query the expired items in the Main Store
expired <- dbGetQuery(con, paste('SELECT Item_ID_Bottle as Item_ID, count(Item_ID_Bottle) AS expireCount FROM Bottle WHERE EXP_DATE >= \'', 
                                 as.Date(orderDate), '\' and EXP_DATE <= \'', as.Date(nextOrder), 
                                 '\' AND Dist_Date = \'0000-00-00\' group by Item_ID_Bottle'))

# query the current stock from the Main Store
currentStock <- dbGetQuery(con, paste('SELECT Item_ID_Bottle as Item_ID, count(Item_ID_Bottle) AS currentStock FROM Bottle WHERE EXP_DATE <= \'', 
                                      as.Date(orderDate), '\'AND Dist_Date = \'0000-00-00\' group by Item_ID_Bottle'))

# disconnect from database
all_cons <- dbListConnections(MySQL())
for (con in all_cons)
  dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------- DECLARATIONS AND PREPARATION  ---------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#


# prepare data and send to prediction algorithm
need <- as.numeric(needpredict(bottles))

# adjust output based on current stock
need <- expiredAdjust(need)


# all data; to be sent to simulated annealing and greedy functions to get final order!
initial <- data.frame("ID" = medID, "Risk" = rep(1, length(medID), replace = TRUE), "Cost" = as.numeric(cost), 
                      "Need" = as.numeric(need), "Ratio" = as.numeric(need/cost))

# cost limit (constraint)
maxCost <- sum(cost*need) * 0.50

# perform simulated annealing
myOrder <- simulatedAnnealingFunc(initial)
myOrder <- cbind(medID, myOrder)

# send simulated annealing output to greedy function
myOrder <- greedyFunc(myOrder)
myOrder <- myOrder[order(myOrder$Need, decreasing = TRUE),]

# remove ratio from output to prepare to send to website
myOrder <- myOrder[,-4]

write.csv(myOrder, file = "pics/myOrder.csv", append = FALSE)


