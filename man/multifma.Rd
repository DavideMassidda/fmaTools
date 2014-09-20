\name{multifmaIC}
\alias{multifmaIC}
\alias{tileplot}

\title{Information Criteria for Multiple Factor Mixture Models}
\description{Fit simultaneously a factor mixture model for \code{k} classes and \code{r} factors.}

\usage{
multifmaIC(dataset, k, r, ...)
tileplot(object, index = "BIC", main = NULL, legend.text = NULL, bw = FALSE)
}

\arguments{
    \item{dataset}{Data frame or matrix with observed data.}
    \item{k}{The number of the mixture components.}
    \item{r}{The number of factors.}
    \item{...}{Further arguments for the function \code{fma}.}
    \item{object}{An object fitted with \code{multifmaIC}.}
    \item{index}{Character string indicating the index to display.}
    \item{main}{Text to use as title for the plot.}
    \item{legend.text}{Text to use as title for legend of the plot.}
    \item{bw}{Logical value indicating if the graph must be filled in black and white.}
}

\details{
~~ Insert details ~~
}

\value{
    The function \code{multifmaIC} returns a list factors*classes; this output can be visualized
    using the function \code{tileplot}, which shows a tileplot for the distribution of a given index.}

\references{
~~ Literature or other references for background information ~~
}

\seealso{
    \code{\link[FactMixtAnalysis]{fma}}
}

\examples{
\dontrun{
data(fmaDemo)
fit <- multifmaIC(fmaDemo, k=1:5, r=1:5)
tileplot(fit)
}
}

\keyword{misc}