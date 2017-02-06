rankhospital <- function(state, outcome, num = "best") {
  
  outcome.data<-read.csv("C:\\Users\\nk177\\Documents\\data_science_coursera\\R-Ch2_HW4\\rprog%2Fdata%2FProgAssignment3-data\\outcome-of-care-measures.csv",colClasses = "character",check.names = FALSE)
  possible_outcomes<-c("heart attack","heart failure","pneumonia")
  
  ## Check that the state and outcome are valid
  if(is.na(match(state,outcome.data$State))){
    stop("invalid state")
  }
  
  if(is.na(match(outcome,possible_outcomes))){
    stop("invalid outcome")
  }
  
  ## Find the column with matched outcome
  col_with_specified_outcome = paste("Hospital 30-Day Death (Mortality) Rates from",outcome)
  matched_column_index <- match(toupper(col_with_specified_outcome),toupper(names(outcome.data)))
  
  ## use the data for relevant state
  reduced_data_with_required_State<-outcome.data[outcome.data$State==state,]
  
  reduced_data_with_required_State_and_No_NA<-(reduced_data_with_required_State[
                          (reduced_data_with_required_State[,
                          matched_column_index]!="Not Available"),])
  
  ## order data with the best rate
  ordered_data<-(reduced_data_with_required_State_and_No_NA[order(
    as.numeric(reduced_data_with_required_State_and_No_NA[,
    matched_column_index]),reduced_data_with_required_State_and_No_NA$`Hospital Name`),])
  
  worst_rank <-nrow(ordered_data)
  if(num=="best" || num==1){
    return_vector=ordered_data$`Hospital Name`[1]
  }
  else if(num=="worst" || num==worst_rank){
    return_vector=ordered_data$`Hospital Name`[worst_rank]
  }
  else
    return_vector=ordered_data$`Hospital Name`[num]
  print(return_vector)
}