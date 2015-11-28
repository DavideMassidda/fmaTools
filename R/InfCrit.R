.CAIC <- function(logL,p,n)
    return(-2*logL + p*(log(n)+1))
.ABIC <- function(logL,p,n)
    return(-2*logL + p*log((n+2)/24))

.fmaIC <- function(models)
{
    numFit <- length(models)
    logL <- n <- p <- k <- rep.int(NA,numFit)
    index <- data.frame(AIC=k, CAIC=k, BIC=k, ABIC=k)
    for(i in 1:numFit) {
        k[i] <- max(models[[i]]$index)
        logL[i] <- models[[i]]$lik[length(models[[i]]$lik)]
        n[i] <- length(models[[i]]$index)
        p[i] <- (models[[i]]$aic+2*logL[i])/2
        index$AIC[i] <- models[[i]]$aic
        index$BIC[i] <- models[[i]]$bic
    }
    index$CAIC <- .CAIC(logL,p,n)
    index$ABIC <- .ABIC(logL,p,n)
    index$ClassNum <- k
    return(index)
}

fmaIC <- function(...)
{
    # Usage:
    # ind <- extractIC(fit1,fit2,fit3,fit4,fit5)
    models <- list(...)
    numFit <- length(models)
    logL <- n <- p <- k <- rep.int(NA,numFit)
    index <- data.frame(AIC=k, CAIC=k, BIC=k, ABIC=k)
    for(i in 1:numFit) {
        k[i] <- max(models[[i]]$index)
        logL[i] <- models[[i]]$lik[length(models[[i]]$lik)]
        n[i] <- length(models[[i]]$index)
        p[i] <- (models[[i]]$aic+2*logL[i])/2
        index$AIC[i] <- models[[i]]$aic
        index$BIC[i] <- models[[i]]$bic
    }
    index$CAIC <- .CAIC(logL,p,n)
    index$ABIC <- .ABIC(logL,p,n)
    index$ClassNum <- k
    return(index)
}

plotIC <- function(object, main="Information criteria",  bw=FALSE)
{
    x <- stack(object[,which(colnames(object)!="ClassNum")])
    colnames(x) <- c("Value","Index")
    x$ClassNum <- rep.int(object$ClassNum,4)
    x$Index <- factor(x$Index,levels=c("AIC","CAIC","BIC","ABIC"))
    x$ClassNum <- factor(x$ClassNum,ordered=TRUE)
    if(!bw)
        pal <- "Set1"
    else
        pal <- 6
    gp <- ggplot(data=x,aes_string(x="ClassNum",y="Value",group="Index",colour="Index"))+geom_point()
    th <- theme(axis.text.x=element_text(colour="black"), legend.direction="horizontal", legend.position="top")
    col <- scale_colour_brewer(palette=pal)
    xl <- xlab("Number of latent classes")
    main <- ggtitle(main)
    gp + geom_point(size=4) + geom_line(size=1) + th + col + xl + main
}
