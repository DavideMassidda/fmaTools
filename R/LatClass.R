plotLC <- function(dataset,fit,fn=mean,ylim=NULL,bw=FALSE)
{
    nLC <- max(fit$index)
    nItem  <- ncol(dataset)
    x <- data.frame(Class=rep(seq_len(nLC),each=nItem))
    itemNames <-colnames(dataset)
    if(is.null(itemNames))
        itemNames <- paste("item",1:nItem,sep="")
    x$Stat <- x$Item <- NA
    for(i in 1:nLC) {
        x$Stat[which(x$Class==i)] <- apply(dataset[which(fit$index==i),],2,fn,na.rm=TRUE)
        x$Item[which(x$Class==i)] <- itemNames
    }
    x$Class <- as.factor(x$Class)
    x$Item <- factor(x$Item,levels=itemNames)
    if(is.null(ylim)) {
        ylim <- range(x$Stat)
        ylim[1] <- floor(ylim[1])
        ylim[2] <- ceiling(ylim[2])
    }
    if(!bw)
        pal <- "Set1"
    else
        pal <- 6
    gp <- ggplot(data=x,aes_string(x="Item",y="Stat",group="Class",colour="Class",ymax=3))
    ln1 <- geom_line(size=2,colour="gray20")
    ln2 <- geom_line(size=1.4)
    pt1 <- geom_point(size=5,colour="gray20")
    pt2 <- geom_point(size=4.4)
    th <- theme(axis.text.x=element_text(angle=45, colour="black"), legend.direction="horizontal", legend.position="top")
    sy <- scale_y_continuous(limits = ylim)
    col <- scale_colour_brewer(palette=pal)
    xl <- xlab("Item")
    yl <- ylab("Score")
    main <- ggtitle("Item scores of classes")
    if(!bw)
        gp + ln2 + pt2 + th + sy + col + xl + yl + main
    else
        gp + ln1 + ln2 + pt1 + pt2 + th + sy + col + xl + yl + main
}
