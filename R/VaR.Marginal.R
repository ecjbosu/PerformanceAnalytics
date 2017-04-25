#' @title VaR.Marginal
#'
#' @description
#' Marginal Value at Risk
#' \code{VaR.Marginal} Calculates the VaR on the margin for a risk factor.
#' 
#' @export
# 7/18/14 Joe W. Byers changes:
# added weightsFlag and changed weightingvector parameter to weights to be consistent
# with VaR method
# commented out Return.portofolio code because Returns should have already been massaged
# in the VaR method
# added at export for temporary debugging.
# added inversion checks
# added subPortVaR to result data.frame
VaR.Marginal <-
function(R, p = 0.95, method=c("modified","gaussian","historical"), weights=NULL, mu=NULL, 
            sigma=NULL, m3=NULL, m4=NULL, invert=TRUE, reasoncheck=FALSE, weightsFlag=TRUE)
{   # @author Brian G. Peterson

    # Description:

    # Function to implement Marginal VaR
    #
    # R                     Returns of your components
    # p                     probability to calculate VaR over
    # modified              use modified VaR or traditional VaR
    # weights       to calculate portfolio returns with
    # weightsFlag   TRUE/FALSE to enable/disable weighting vector multiplier
    #
    # @returns data frame with total VaR of the portfolio plus Marginal VaR for each component

    #invert by default for marginal VaR in the VaR calls if False will be re-inverted at the end

    R = checkData(R, method = "xts")

    if (is.null(weights)) {
        weights = t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    }
    
    if(method==TRUE) method = "modified"

    #if (ncol(weightingvector) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")

    columns = ncol(R)
    columnnames=c("PortfolioVaR",colnames(R))

    # Function

    # first, get the numbers for the whole portfolio
    # call VAR with clean = "none" since clean should have been done before this call.
#    portfolioR   = Return.portfolio(R,as.vector(weightingvector))
#    portfolioVaR = VaR(portfolioR,p,method,portfolio_method="single", mu, sigma, m3, m4, invert, reasoncheck)
    portfolioVaR = VaR(R=R,p=p,method=method,portfolio_method="single", weights=weights, 
                 mu=mu, sigma=sigma, m3=m3, m4=m4, invert=T, reasoncheck=reasoncheck)
    pVaR = array(portfolioVaR)
    result=data.frame(mVaR=pVaR, subPVaR=pVaR)

    for(column in 1:columns) {
        # calculate a multiplication factor, because the results don't seem to make sense
        # unless the weighting vector always equals the same sum
        weightfactor <- 1;
        if (weightsFlag) weightfactor = sum(weights)/sum(t(weights)[,-column])  # if we do need it
        # weightfactor = 1  # if we don't need it

        subportfolioVaR <- 0;
#        subportfolioR   = Return.portfolio(R[ ,-column],as.vector(t(weightingvector)[ ,-column]*weightfactor))
#        subportfolioVaR = VaR(subportfolioR,p,method,portfolio_method="single", mu, sigma, m3, m4, invert, reasoncheck)

        # check weights for at least one non-zero column.
        if (any(weights[-column]!=0))     
        
        subportfolioVaR = VaR(R=R[ ,-column],P=p,method=method,portfolio_method="single", 
                          weights=as.vector(t(weights)[ ,-column]*weightfactor), mu=mu[-column], 
                          sigma=sigma[-column,-column], m3=m3[-column,-column], m4=m4[-column,-column], 
                          invert=T, reasoncheck=reasoncheck)

        marginalVaR = subportfolioVaR - portfolioVaR

        mVaR = array(marginalVaR)
        mVaR = data.frame(mVaR=mVaR, subPVaR=array(subportfolioVaR))

        result=rbind(result,mVaR)
    } #end columns loop

    rownames(result)<-columnnames

    if (invert==F) result <- -result;
    # Return Value:
    return(result)
} # end function VaR.Marginal

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: VaR.Marginal.R 3301 2014-01-18 15:26:12Z braverock $
#
###############################################################################