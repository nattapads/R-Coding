COV <- function(file.csv) {
        library(xts)
        Data.df <- read.csv(file.csv)
        Data.df$main_ingredient <- as.character(Data.df$main_ingredient)
        Inv_List <- unique(Data.df$main_ingredient)
        Inv_List <- as.character(Inv_List)
        Output <- data.frame()
        for (i in 1:length(Inv_List)){
                Menu_List <- subset(Data.df, main_ingredient == Inv_List[i])
                Menu_List[,2] <- as.Date(Menu_List$create_date,format='%d/%m/%Y')
                Menu_List <- aggregate(quantity ~ create_date, Menu_List, sum)
                Menu_List <- as.xts(Menu_List$quantity,order.by=as.Date(Menu_List$create_date))
                Menu_List <- apply.weekly(Menu_List,sum)
                Menu_List <- data.frame(Inv_List[i], 
                                        round(mean(Menu_List), digits = 0), 
                                        round(sd(Menu_List), digits = 0),
                                        round(100*(sd(Menu_List)/mean(Menu_List)), digits = 2))
                Output <- rbind(Output, Menu_List)
        }
        colnames(Output)[1:4] <- c("Inventory","Average","SD","COV")
        Output <- Output[with(Output, order(COV)), ]
        Output
}