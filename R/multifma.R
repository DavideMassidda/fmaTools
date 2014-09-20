multifmaIC <- function(dataset,k,r,...)
{
    nk <- length(k)
    nr <- length(r)
    fit <- val <- vector("list",nr)
    names(fit) <- names(val) <- paste("r =",r)
    fit_k <- vector("list",nk)
    names(fit_k) <- paste("k =",k)
    for(i in 1:nr) {
        fit[[i]] <- fit_k
        for(j in 1:nk) {
            cat("factors:",r[i],"classes:",k[j],"\n")
            fit[[i]][[j]] <- fma(dataset,k=k[j],r=r[i],...)
        }
        val[[i]] <- .fmaIC(fit[[i]])
    }
    return(val)
}

tileplot <- function(object, index="BIC", main=NULL, legend.text=NULL, bw=FALSE)
{
    val <- NULL
    fct <- as.numeric(gsub("r =","",names(object)))
    for(i in 1:length(object)) {
        object[[i]]$FactNum <- fct[i]
        val <- rbind(val,object[[i]])
    }
    val$ClassNum <- factor(val$ClassNum,ordered=TRUE)
    if(is.null(legend.text)) legend.text <- "Value"
    if(is.null(main)) main <- index
    gp <- ggplot(val,aes_string(x="ClassNum",y="FactNum",fill=index))
    sq <- geom_tile(aes_string(fill=index),colour="white")
    th <- theme(axis.text=element_text(colour="black"))
    if(!bw)
        gr <- scale_fill_gradient(low="#a0d2fa",high="#321450")
    else
        gr <- scale_fill_gradient(low="#cdcdcd",high="#323232")
    lg <- guides(fill=guide_legend(title=legend.text))
    xl <- xlab("Number of classes")
    yl <- ylab("Number of factors")
    main <- ggtitle(main)
    gp + sq + gr + th + xl + yl + lg + main
}
