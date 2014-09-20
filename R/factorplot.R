factorplot <- function(fit, main=NULL, legend.text=NULL, bw=FALSE)
{
    if(is.null(main)) main <- "Factor scores"
    if(is.null(legend.text)) legend.text <- "Class"
    if(!bw)
        pal <- "Set1"
    else
        pal <- 6
    val <- NULL
    for(i in 1:ncol(fit$z)) {
        x <- aggregate(fit$z[,i]~fit$index,FUN=mean)
        colnames(x) <- c("group","mean")
        x$sd <- aggregate(fit$z[,1]~fit$index,FUN=sd)[,2]
        x$num <- aggregate(fit$z[,1]~fit$index,FUN=length)[,2]
        x$factor <- i
        val <- rbind(val,x)
    }
    val$se <- val$sd/sqrt(val$num)
    val$low <- val$mean-val$se
    val$high <- val$mean+val$se
    val$group <- as.factor(val$group)
    val$factor <- as.factor(val$factor)
    gp <- ggplot(val,aes_string(x="factor",y="mean",group="group",fill="group"))
    bar <- geom_bar(stat="identity",position="dodge",colour="black")
    dodge <- position_dodge(width=0.9)
    err <- geom_errorbar(aes_string(ymin="low",ymax="high"),width=0.25,size=0.8,colour="black",position=dodge)
    th <- theme(axis.text=element_text(colour="black"))
    leg <- guides(fill=guide_legend(title=legend.text))
    pal <- scale_fill_brewer(palette=pal)
    xl <- xlab("Factor")
    yl <- ylab("Mean of factor scores (z scale)")
    main <- ggtitle(main)
    gp + bar + err + leg + xl + yl + main + th + pal
}
