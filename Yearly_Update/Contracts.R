#Contracts
nflData <- read.csv("/Users/paulgallagher/Documents/NFL Draft/2021/NFLData.csv",stringsAsFactors = FALSE)
contracts <- nflreadr::load_contracts()
contracts <- contracts[contracts$year_signed >= 2015,]
contracts <- contracts[is.na(contracts$draft_year) == FALSE,]
contracts <- contracts[is.na(contracts$draft_overall) == FALSE,]
contracts <- contracts[is.null(contracts$cols) == FALSE,]
contracts <- contracts %>% distinct(player, .keep_all = TRUE)

yearly_contracts <- NULL
yearly_contracts <- cbind(contracts[1,c(1:2,5,20:23)],contracts[[24]][[1]])

for (i in 2:nrow(contracts)){
  if(is.null(contracts[[24]][[i]]) == TRUE){
    i = i + 1
  }else{
    temp_df <- NULL
    temp_df <- cbind(contracts[i,c(1:2,5,20:23)],contracts[[24]][[i]])
    yearly_contracts <- rbind(yearly_contracts,temp_df)
    #print(i)
  }
}
yearly_contracts_backup <- yearly_contracts
yearly_contracts <- yearly_contracts[yearly_contracts$year != 'Total',]
yearly_contracts$contract_id <- paste(yearly_contracts$draft_year,yearly_contracts$draft_overall,yearly_contracts$year,sep = '-')
nflData$contract_id <- paste(nflData$DraftYear,nflData$Pick,nflData$Year,sep = '-')

yearly_contracts <- merge(yearly_contracts,nflData, by = 'contract_id')
yearly_contracts$AV_per_cap_percent <- yearly_contracts$AV / yearly_contracts$cap_percent
yearly_contracts <- yearly_contracts[is.na(yearly_contracts$AV_per_cap_percent) == FALSE,]
yearly_contracts <- yearly_contracts[yearly_contracts$cap_percent > 0,]

contract_expectations <- yearly_contracts %>% filter(Season > 4) %>% group_by(Pos) %>% summarise(AV_per_cap_percent = median(AV_per_cap_percent))

write.csv(contract_expectations,"Datasets/contract_expectations.csv")
