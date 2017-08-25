Code for the Hackathon - The Seers Accuracy by Analytics Vidhya

####Objective The objective of the competition is to predict whether the customer will come back in the next one year or not.

####Approach I had transaction data of all the customers from Jan 2003 to Dec 2006. The idea is to predict whether the customer will come back in 2007 or not.

    The first step was to create a proper validation framework since there was no "target" variable
    I have used transaction data from 2003 to 2005 to create the features.
    People who came back in 2006 were tagged as 1 and others were tagged as 0, thereby getting the target column
    Feature selection, models tuning were done using this validation sample.
    For the final model, features were created using all the given data (2003 to 2006) and prediction was done for 2007.
    
