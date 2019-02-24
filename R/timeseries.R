#' @title Produce a time series that counts up or otherwise summarises artefact data per year  
#'
#' @description Given a set of archaeological artefacts with either estimated absolute start and end dates (e.g. based on their style) or a probability model for their likely production intensity through time), count up how many artefacts are definitely or possibly present for each year in a specified time range. 
#' 
#' @param startdate A vector of numeric values specifying the approximate start dates for the production of each artefact.
#' @param enddate A vector of numeric values specifying the approximate end dates for the production of each artefact.
#' @param weight A single numeric vector >0 to be used in weighting the counts or other estimates per year (e.g. object size or weight)..
#' @param timeRange Numeric vector of length 2, specifying the earliest and latest date to include in the results (in the year units of the calendar argument). The default option will calculate this automatically from the input data.
#' @param calendar Specifies whether the input calendar is in years BC (-ve)/AD (+ve) or BP (default is "BCAD"). Note there should be no year 0 in "BCAD".
#' @param verbose A logical variable indicating whether extra information on progress should be reported. Default is TRUE.
#' 
#' @return A data.frame with the number artefacts whose production range overlaps with that year, an aoristic sum and the same adjusted for weight.
#'
#' @references #' Ratcliffe, J.H. 2000. Aoristic analysis: the spatial interpretation of unspecifed temporal events, International Journal of Geographical Information Science 14: 669–679.x#'
#' Johnson, I. 2004. Aoristic analysis: seeds of a new approach to mapping archaeological distributions through time, In K.F. Ausserer, W. Börner, M. Goriany and L. Karlhuber-Vöckl (eds.), [Enter the past]. The E-way into the Four Dimensions of Cultural Heritage (CAA 2003): 448–452. Oxford: Archaeopress.x#'
#' @export
aoristos <- function(startdate, enddate, weight=NA, timeRange=NA, calendar="BCAD", verbose=TRUE){

    ## Initial checks    
    if (!is.na(timeRange[1])){
        if (length(timeRange)!=2){
            stop("timeRange must be either NA (default) or a numeric vector of length 2 specifying the oldest and youngest date in the desired range.")
        }
    }
    if (calendar=="BCAD"){
        years <- seq(min(startdate)+1,max(enddate),1)
        years <- years[years !=0]
        if (!is.na(timeRange[1])){
            years <- years[years >= timeRange[1] & years <= timeRange[2]]
        }
        acounts <- data.frame(YearBCAD=years, Overlap=0, AorSum=0)
    } else if (calendar=="BP"){
        years <- seq(max(startdate)-1,min(enddate),1)
        if (!is.na(timeRange[1])){
            years <- years[years <= timeRange[1] & years >= timeRange[2]]
        }
        acounts <- data.frame(YearBP=years, Overlap=0, AorSum=0)
    } else {
        stop("calendar argument must be either \"BCAD\" or \"BP\".")
    }
    if (length(startdate) != length(enddate)){
        stop("The arguments startdate and enddate must be of the same length.")
    }
    if (!is.na(weight[1])){
        if (length(startdate) != length(weight)){
            stop("The argument weight must the same length as startdate.")
        }
        if (min(weight) < 0){
            stop("weight argument must be greater than 0.")
        }
        acounts$OverlapW <- 0
        acounts$AorSumW <- 0
    }
    if (length(startdate)>1 & verbose){
        print("Working on main artefact calculations...")
        flush.console()
        pb <- txtProgressBar(min=1, max=length(startdate), style=3)
    }
    for (a in 1:length(startdate)){
        if (length(startdate)>1 & verbose){ setTxtProgressBar(pb, a) }
        ayears <- years <= enddate[a] & years > startdate[a]
        acounts[ayears, "Overlap"] <- acounts[ayears, "Overlap"] + 1
        yearrange <- abs(enddate[a] - startdate[a])
        acounts[ayears, "AorSum"] <- acounts[ayears, "AorSum"] +(1/yearrange)
        if (!is.na(weight[1])){
            acounts[ayears, "OverlapW"] <- acounts[ayears, "Overlap"] * weight[a]
            acounts[ayears, "AorSumW"] <- (acounts[ayears, "AorSum"] +(1/yearrange)) * weight[a]
        }
    }
    if (length(startdate)>1 & verbose){
        close(pb)
    }
    class(acounts) <- append(class(acounts),"aCounts")
    if (length(startdate)>1 & verbose){
        print("Done.")
    }
    return(acounts)
}

