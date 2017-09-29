PLATO2ESRI <- function(Orders, Mappoints, Routes) {
    O_vars <- c('X','Y','MUSTGOWEIGHT','SITENAME','DATEFROM','DATETO')
    M_vars <- c('MAPPOINT','X','Y','MPCLASSID')
    R_vars <- c('X','Y','UNIT3_CAPACITY','MAPPOINT','VEHICLE')
    
    O_data <- Orders[,O_vars]
    M_data <- Mappoints[,M_vars]
    R_data <- Routes[,R_vars]
    
    EST <- 9999999999999
    LST <- 0
    
    ### orders
    k <- nrow(O_data)
    t1 <- O_data$DATEFROM
    t2 <- O_data$DATETO
    
    odr <- '&orders={"features":['
    for (j in 1:k) { 
        if (j < k) {
            x = CC( O_data[j,"X"] )
            y = CC( O_data[j,"Y"] )
            DQ <- O_data[j,"MUSTGOWEIGHT"]
            Name <- O_data[j,"SITENAME"]
            if (grepl('&',Name) == 1) {
               next
            }
            TWS1 <- as.numeric(as.POSIXct( t1[j] )) * 1000
            if (EST > TWS1) {EST = TWS1}
            TWE2 <- as.numeric(as.POSIXct( t2[j] )) * 1000
            if (LST < TWE2) {LST = TWE2}
            geo <- paste('{"geometry":{"x":', x, ',', '"y":', y, '},', sep = "")
            att <- paste('"attributes":{"DeliveryQuantities":', DQ,',"Name":"', Name,'","ServiceTime":', 25,',"TimeWindowStart1":', TWS1,',"TimeWindowEnd1":', TWE2,',"MaxViolationTime1":', 100,'}},',sep = "")
            
            odr <- paste(odr, geo, att, sep = "") 
        } else {
            x = CC( O_data[j,"X"] )
            y = CC( O_data[j,"Y"] )
            DQ <- O_data[j,"MUSTGOWEIGHT"]
            Name <- O_data[j,"SITENAME"]
            if (grepl('&',Name) == 1) {
                next
            }
            TWS1 <- as.numeric(as.POSIXct( t1[j] )) * 1000
            if (EST > TWS1) {EST = TWS1}
            TWE2 <- as.numeric(as.POSIXct( t2[j] )) * 1000
            if (LST < TWE2) {LST = TWE2}
            geo <- paste('{"geometry":{"x":', x, ',', '"y":', y, '},', sep = "")
            att <- paste('"attributes":{"DeliveryQuantities":', DQ,',"Name":"', Name,'","ServiceTime":', 25,',"TimeWindowStart1":', TWS1,',"TimeWindowEnd1":', TWE2,',"MaxViolationTime1":', 100,'}}]}',sep = "")
            
            odr <- paste(odr, geo, att, sep = "")
        }
    }
    
    ### routes
    k <- nrow(R_data)
    LST <- EST + 60000
    
    Depot_Names <- zeros (k,1)
    rts <- '&routes={"features":['
    for (j in 1:k) { 
        if (j < k) {
            x = CC( R_data[j,"X"] )
            y = CC( R_data[j,"Y"] )
            Name <- R_data[j,"VEHICLE"]
            if (grepl('&',Name) == 1) {
                next
            }
            DepotName <- R_data[j,"MAPPOINT"]
            
            # if ( sum(M_data$MAPPOINT == DepotName) == 0 ){
            #     next
            # }
            Depot_Names[j] <- DepotName
            Capacity <- R_data[j,"UNIT3_CAPACITY"]
            
            #geo <- paste('{"geometry":{"x":', x, ',', '"y":', y, '},', sep = "")
            att <- paste('{"attributes":{"Name":"', Name,'","StartDepotName":"', DepotName,'","EndDepotName":"', DepotName,'","EarliestStartTime":', EST,',"LatestStartTime":', LST,',"Capacities":"', Capacity,'","CostPerUnitTime":', 0.2,',"CostPerUnitDistance":', 1.5,',"MaxOrderCount":', 200,',"MaxTotalTime":', 3600,',"MaxTotalTravelTime":', 1200,',"MaxTotalDistance":', 800,'}},',sep = "")

            #rts <- paste(rts, geo, att, sep = "") 
            rts <- paste(rts, att, sep = "") 
        } else {
            x = CC( R_data[j,"X"] )
            y = CC( R_data[j,"Y"] )
            Name <- R_data[j,"VEHICLE"]
            if (grepl('&',Name) == 1) {
                next
            }
            DepotName <- R_data[j,"MAPPOINT"]
            
            # if ( sum(M_data$MAPPOINT == DepotName) == 0 ){
            #     next
            # }
            Depot_Names[j] <- DepotName
            Capacity <- R_data[j,"UNIT3_CAPACITY"]
            
            #geo <- paste('{"geometry":{"x":', x, ',', '"y":', y, '},', sep = "")
            att <- paste('{"attributes":{"Name":"', Name,'","StartDepotName":"', DepotName,'","EndDepotName":"', DepotName,'","EarliestStartTime":', EST,',"LatestStartTime":', LST,',"Capacities":"', Capacity,'","CostPerUnitTime":', 0.2,',"CostPerUnitDistance":', 1.5,',"MaxOrderCount":', 200,',"MaxTotalTime":', 3600,',"MaxTotalTravelTime":', 1200,',"MaxTotalDistance":', 800,'}}]}',sep = "")
            #att <- paste('{"attributes":{"Name":"', Name,'","StartDepotName":"', DepotName,'","EndDepotName":"', DepotName,'","Capacities":', Capacity,',"CostPerUnitTime":', 0.2,',"CostPerUnitDistance":', 1.5,',"MaxOrderCount":', 200,',"MaxTotalTime":', 3600,',"MaxTotalTravelTime":', 1200,',"MaxTotalDistance":', 800,'}}]}',sep = "")
            # "EarliestStartTime":', <>,',
            # "LatestStartTime":', <>,'
            
            #rts <- paste(rts, geo, att, sep = "") 
            rts <- paste(rts, att, sep = "") 
        }
    }
    
    ### depots
    Depot_Names <- unique(Depot_Names)
    print(Depot_Names)
    M_data <- M_data[M_data$MPCLASSID == 2,]
    ind <- (1:nrow(M_data))
    k <- length(Depot_Names)
    
    idx <- zeros (k,1)
    ii <- 1
    for (i in 1:k){
        if ( sum(M_data$MAPPOINT == Depot_Names[i]) != 0 ) {
            idx[ii] <- ind[M_data$MAPPOINT == Depot_Names[i]]
            
            ii <- ii - 1
        }
        ii <- ii + 1
    }
    print (idx)
    
    dpt <- '&depots={"features":['
    for (j in 1:k) { 
        if (j < k) {
            x = CC( M_data[j,"X"] )
            y = CC( M_data[j,"Y"] )
            Name <- M_data[j,"MAPPOINT"]
            
            geo <- paste('{"geometry":{"x":', x, ',', '"y":', y, '},', sep = "")
            att <- paste('"attributes":{"Name":"', Name,'"}},',sep = "")
            # "TimeWindowStart1":', TWS1,', Do not forget comma!
            # "TimeWindowEnd1":', TWE1,',
            # "MaxViolationTime1":', 0,'
            dpt <- paste(dpt, geo, att, sep = "") 
        } else {
            x = CC( M_data[j,"X"] )
            y = CC( M_data[j,"Y"] )
            Name <- M_data[j,"MAPPOINT"]
            
            geo <- paste('{"geometry":{"x":', x, ',', '"y":', y, '},', sep = "")
            att <- paste('"attributes":{"Name":"', Name,'"}}]}',sep = "")
            # "TimeWindowStart1":', TWS1,', Do not forget comma!
            # "TimeWindowEnd1":', TWE1,',
            # "MaxViolationTime1":', 0,'
            
            dpt <- paste(dpt, geo, att, sep = "")
        }
    }
    
    return (list(odr,dpt,rts))
}