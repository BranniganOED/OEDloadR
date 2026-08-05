% OED_QCEW_Table documentation

\name{OED_QCEW_Table}
\alias{OED_QCEW_Table}
\title{Download or Read Oregon QCEW Tables}
\description{
Download or read QualityInfo Quarterly Census of Employment and Wages
workbooks and return a standardized table. The default request is the Oregon
statewide annual table, and valid geographies and years can be inspected with
\code{OED_QCEW_Options()}.
}
\usage{
OED_QCEW_Table(
  Year = NULL,
  Period = "Annual",
  Years = Year,
  Periods = Period,
  Geographies = "Oregon",
  NAICS = NULL,
  NAICSMatch = c("exact", "prefix"),
  Ownership = "All Ownerships",
  ReportType = "All Industries",
  IndustryLevel = "Total, all industries",
  IndustrySuperSector = "0000",
  IndustrySector = "00",
  Industry = "0000",
  DownloadDir = NULL,
  Overwrite = FALSE,
  MaxRequests = 100,
  PreviewOnly = FALSE,
  ReportNAICSAvailability = !is.null(NAICS),
  KeepDownloadMetadata = FALSE,
  Paths = NULL,
  Sheets = NULL,
  keep_raw_values = FALSE,
  Refresh = c("auto", "always", "never"),
  MaxAge = NULL
)
}
\arguments{
\item{Year}{Optional single year retained for compatibility with the
\code{Years} argument.}
\item{Period}{Optional single period retained for compatibility with the
\code{Periods} argument.}
\item{Years}{Years to request, or \code{"all"}.}
\item{Periods}{Periods to request, including \code{"Annual"}, quarters,
\code{"all"}, or \code{"quarterly"}.}
\item{Geographies}{Optional Oregon geography names, short names, or QCEW area
codes. The default is the Oregon statewide area.}
\item{NAICS}{Optional NAICS codes to retain.}
\item{NAICSMatch}{Whether NAICS matching is exact or prefix-based.}
\item{Ownership}{QCEW ownership label or code.}
\item{ReportType}{QCEW report type label or code. The default is
\code{"All Industries"}, which is the QualityInfo report for the statewide
total.}
\item{IndustryLevel}{Requested QCEW industry level.}
\item{IndustrySuperSector}{QCEW supersector code.}
\item{IndustrySector}{QCEW sector code.}
\item{Industry}{QCEW industry code.}
\item{DownloadDir}{Directory used for downloaded QCEW workbooks. When
omitted, the shared \code{OEDloadR data/QCEW} folder is used.}
\item{Overwrite}{Whether existing QCEW workbooks should be downloaded again.}
\item{Refresh}{Cache policy: \code{"auto"} uses seven days for the latest
available QCEW period and 90 days for older fixed periods; \code{"always"}
downloads a fresh workbook; and \code{"never"} reuses a valid local workbook.
\code{Overwrite = TRUE} is retained as an alias for \code{Refresh = "always"}.}
\item{MaxAge}{Optional maximum cache age in days that overrides the automatic
QCEW rule.}
\item{MaxRequests}{Maximum number of QualityInfo requests allowed.}
\item{PreviewOnly}{Return a request plan without downloading workbooks.}
\item{ReportNAICSAvailability}{Whether to report requested NAICS availability.}
\item{KeepDownloadMetadata}{Whether to retain workbook download metadata.}
\item{Paths}{Optional previously downloaded QCEW workbook paths. When
omitted, the function uses the QualityInfo web source.}
\item{Sheets}{Optional workbook sheet names.}
\item{keep_raw_values}{Whether to retain raw metric values beside parsed values.}
}
\value{
A standardized data frame. Download results carry \code{download_plan} and
\code{download_diagnostics} attributes and may also include
\code{"failed_requests"} and \code{"naics_availability"} attributes.
}
\details{
The standard request uses bundled QualityInfo geography and year metadata and
does not load the QCEW webpage or lookup services before downloading a report.
QCEW still uses one report request per requested geography/year/period because
the selected-area report endpoint does not provide a suitable bulk replacement.

When \code{DownloadDir} is omitted, QCEW workbooks are stored in the shared
\code{OEDloadR data/QCEW} folder. The root defaults to
\code{file.path(getwd(), "OEDloadR data")} and can be overridden with
\code{options(OEDloadR.data_dir = "D:/custom/path")} or the
\code{OEDLOADR_DATA_DIR} environment variable. Downloaded workbooks have a
JSON metadata sidecar. Automatic refresh uses the current QualityInfo period
and age rules; server revision headers are used when the source provides them.
}
\examples{
\dontrun{
library(OEDloadR)
OED_QCEW_Table(
  Geographies = "Washington",
  Years = 2024,
  Periods = "Annual",
  NAICS = "54",
  NAICSMatch = "prefix",
  PreviewOnly = TRUE
)
}
}
