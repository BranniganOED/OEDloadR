% OED_LAUS_Table documentation

\name{OED_LAUS_Table}
\alias{OED_LAUS_Table}
\title{Download or Read Oregon LAUS Tables}
\description{
Read Oregon Local Area Unemployment Statistics workbooks and standardize
their core labor-market measures.
}
\usage{
OED_LAUS_Table(
  Geographies = NULL,
  Paths = NULL,
  Urls = NULL,
  DownloadDir = file.path("output", "qualityinfo_laus"),
  Overwrite = FALSE,
  PreviewOnly = FALSE,
  MaxDownloads = 10,
  Sheets = NULL,
  PageUrl = "https://www.qualityinfo.org/uesti",
  StartYear = NULL,
  EndYear = NULL
)
}
\arguments{
\item{Geographies}{Optional QualityInfo geography names or area codes. If
omitted, the live loader uses Oregon statewide data. Use
\code{OED_LAUS_Options()} to inspect current values.}
\item{Paths}{Optional previously downloaded LAUS workbook paths.}
\item{Urls}{Optional direct workbook URLs.}
\item{DownloadDir}{Directory for downloaded workbooks.}
\item{Overwrite}{Whether existing workbooks may be replaced.}
\item{PreviewOnly}{Return a download plan without downloading.}
\item{MaxDownloads}{Maximum number of downloads allowed.}
\item{Sheets}{Optional workbook sheet names.}
\item{PageUrl}{QualityInfo unemployment statistics page.}
\item{StartYear}{Optional first year for the live report. Provide together
with EndYear; if both are omitted, the latest available year is used.}
\item{EndYear}{Optional last year for the live report. Provide together
with StartYear.}
}
\value{
A standardized data frame with date, unemployment rate, labor force,
employed, unemployed, geography, and provisional-status fields. Downloaded
results carry \code{download_plan} and \code{download_diagnostics} attributes.
}
\examples{
\dontrun{
library(OEDloadR)
OED_LAUS_Table(
  Geographies = "Washington County"
)
OED_LAUS_Table(
  Geographies = "Washington County",
  StartYear = 2020,
  EndYear = 2025
)
}
}
