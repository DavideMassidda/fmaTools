\name{factorplot}
\alias{factorplot}

\title{Plot Factor Scores}
\description{Plot the factor scores (z scale) of factors for each latent class using a barplot.}

\usage{
factorplot(fit, main = NULL, legend.text = NULL, bw = FALSE)
}

\arguments{
    \item{fit}{Output of the function \code{fma}.}
    \item{main}{Text to use as title for the plot.}
    \item{legend.text}{Text to use as title for legend of the plot.}
    \item{bw}{Logical value indicating if the graph must be filled in black and white.}
}

\details{Error bars represent the standard errors.}

\value{A graphical output showing the factor scores.}

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
factorplot(fit3)
}
}

\keyword{misc}