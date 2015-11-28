\name{plotLC}
\alias{plotLC}

\title{Plot Latent Classes}
\description{Plot the item scores of latent classes estimated by the function \code{fma}.}

\usage{
plotLC(dataset, fit, fn=mean, ylim = NULL, bw = FALSE)
}

\arguments{
    \item{dataset}{
        Data frame containing raw data.
    }
    \item{fit}{
        Output of the function \code{fma}.
    }
    \item{fn}{
        Function scoring for items.
    }
    \item{ylim}{
        Limits of vertical axis for the graph.
    }
    \item{bw}{
        Logical value indicating if the graph must be filled in black and white.
    }
}

\value{A graphical output showing the response pattern of each class.}

\references{
~~ Literature or other references for background information ~~
}

\seealso{
    \code{\link[FactMixtAnalysis]{fma}}
}

\examples{
\dontrun{
data(fmaDemo)
fit3 <- fma(fmaDemo, k=3, r=2)
plotLC(fmaDemo, fit3)
}
}

\keyword{misc}
