COV <- function(directory) {
        # Enable xts package
        library(xts)
        
        # Import and merge file.csv from directory
        files_list <- list.files(directory, full.names=TRUE)
        no_of_files <- length(files_list)
        Data.df <- data.frame()
        for(h in 1:no_of_files) {
                Data.df <- rbind(Data.df, read.csv(files_list[h]))
        }
        
        # Change the type of object in main_ingredient and create_date
        Data.df$main_ingredient <- as.character(Data.df$main_ingredient)
        Data.df$create_date <- as.Date(Data.df$create_date,format='%d/%m/%Y')
        
        # Create a list of distinct inventories
        Inv_List <- unique(Data.df$main_ingredient)
        Inv_List <- as.character(Inv_List)
        
        # Create all dates to replace missing dates for particular inventories
        All_dates = seq(as.Date(as.yearmon(min(Data.df$create_date))), 
                        as.Date(as.yearmon(max(Data.df$create_date))), by="day")
        
        # Create emtry an data frame
        Output <- data.frame()
        
        # Calculate weekly means, SDs and COVs of individual ingredients
        for (i in 1:length(Inv_List)){
                Menu_List <- subset(Data.df, main_ingredient == Inv_List[i])
                Menu_List[,2] <- as.Date(Menu_List$create_date,format='%d/%m/%Y')
                Menu_List <- aggregate(quantity ~ create_date, Menu_List, sum)
                
                #Add missing dates
                Menu_List = merge(data.frame(date = All_dates), Menu_List,
                                  by.x='date', by.y='create_date',
                                  all.x=T, all.y=T)
                colnames(Menu_List)[1:2] <- c("create_date","quantity")
                Menu_List[is.na(Menu_List)] <- 0
                
                # Use xts package to aggregate daily into weekly demand
                Menu_List <- as.xts(Menu_List$quantity,order.by=as.Date(Menu_List$create_date))
                Menu_List <- apply.weekly(Menu_List,sum)
                colnames(Menu_List)[1] <- "Weekly"
                
                # Identify any outlier
                IQRs <- quantile(Menu_List$Weekly, prob = 0.75) - 
                        quantile(Menu_List$Weekly, prob = 0.25)
                Lower <- quantile(Menu_List$Weekly, prob = 0.25) - 
                        1.5*IQRs
                Upper <- quantile(Menu_List$Weekly, prob = 0.75) + 
                        1.5*IQRs
                
                # Treat the outlier by applying 5-day centred moving average
                for (l in 1:length(Menu_List[,1])){
                        if (Menu_List[l,1] < Lower) {
                                Menu_List[l,1] <- round(mean(Menu_List[l-2:l+2,1], digits = 2))
                        } else if (Menu_List[l,1] > Upper) {
                                Menu_List[l,1] <- round(mean(Menu_List[l-2:l+2,1], digits = 2))
                        } else if (Menu_List[l,1] == 0) {
                                Menu_List[l,1] <- 0
                        } else {
                                Menu_List[l,1] <- Menu_List[l,1]
                        }
                }
                
                # Calculate weekly means, SDs and COVs of individual ingredients
                Menu_List <- data.frame(Inv_List[i], 
                                        round(mean(Menu_List), digits = 0), 
                                        round(sd(Menu_List), digits = 0),
                                        round(100*(sd(Menu_List)/mean(Menu_List)), 
                                              digits = 2))
                
                # Add the results to data frame
                Output <- rbind(Output, Menu_List)
        }
        
        # Rename columns
        colnames(Output)[1:4] <- c("Inventory","Weekly Avg.","SD","COV")
        
        # Sort ascending COV
        Output <- Output[with(Output, order(COV)), ]
        
        # Print the result
        Output
}

## Test
COV("BK")