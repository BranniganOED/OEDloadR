% QualityInfo option helpers

\name{OED_QCEW_Options}
\alias{OED_QCEW_Options}
\alias{OED_LAUS_Options}
\alias{OED_Diagnostics}
\title{Discover QualityInfo Options}
\description{
Return the bundled QualityInfo geography, year, and filter choices used by the
QCEW and LAUS command families. The standard helpers are offline so they do
not add discovery requests to a data download.
}
\usage{
OED_QCEW_Options()

OED_LAUS_Options(PageUrl = "https://www.qualityinfo.org/uesti")

OED_Diagnostics(x)
}
\arguments{
\item{PageUrl}{QualityInfo LAUS page. The standard page uses bundled endpoint
and lookup metadata; a custom page uses the dynamic maintenance fallback.}
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
