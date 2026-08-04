% QualityInfo option helpers

\name{OED_QCEW_Options}
\alias{OED_QCEW_Options}
\alias{OED_LAUS_Options}
\alias{OED_Diagnostics}
\title{Discover QualityInfo Options}
\description{
Return current QualityInfo geography, year, and filter choices used by the
QCEW and LAUS command families. The helpers query QualityInfo services rather
than reproducing static lists in the package.
}
\usage{
OED_QCEW_Options()

OED_LAUS_Options(PageUrl = "https://www.qualityinfo.org/uesti")

OED_Diagnostics(x)
}
\arguments{
\item{PageUrl}{QualityInfo LAUS page used to discover the live report and
service endpoints.}
\item{x}{A result returned by one of the OEDloadR download commands.}
}
\value{
\code{OED_QCEW_Options()} returns a named list containing current
\code{geographies}, \code{years}, \code{periods}, \code{ownerships},
\code{report_types}, and \code{industry_levels}.

\code{OED_LAUS_Options()} returns a named list containing current
\code{geographies}, area codes, available \code{years}, and \code{page_url}.

\code{OED_Diagnostics()} extracts the common download summary from a loader
result, or returns \code{NULL} when no download was performed.
}
\examples{
\dontrun{
qcew_options <- OED_QCEW_Options()
head(qcew_options$geographies)

laus_options <- OED_LAUS_Options()
head(laus_options$geographies)
}
}
