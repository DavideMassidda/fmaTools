\name{fmaIC}
\alias{fmaIC}
\alias{plotIC}

\title{Information criteria of factor mixture models}
\description{Description}

\usage{
fmaIC(...)
plotIC(object, main = "Information criteria", bw = FALSE)
}

\arguments{
    \item{...}{
        Objects containing fitted models with the function \code{fma}.
    }
    \item{object}{
        An object returned by the function \code{plotIC}.
    }
    \item{main}{
        Character: the title of the plot.
    }
    \item{bw}{
        Logical value indicating if the graph must be filled in black and white.
    }
}

\value{
    To describe
}

\references{
~~ Literature or other references for background information ~~
}

\seealso{
    \code{\link[FactMixtAnalysis]{fma}}
}

\examples{
\dontrun{
data(fmaDemo) # Two-factors questionnaire
fit1 <- fma(fmaDemo, k=1, r=2)
fit2 <- fma(fmaDemo, k=2, r=2)
fit3 <- fma(fmaDemo, k=3, r=2)
fit4 <- fma(fmaDemo, k=4, r=2)
fit5 <- fma(fmaDemo, k=5, r=2)
information <- fmaIC(fit1,fit2,fit3,fit4,fit5)
plotIC(information)
}
}

\keyword{misc}
