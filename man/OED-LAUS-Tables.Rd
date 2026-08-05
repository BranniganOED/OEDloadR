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
  DownloadDir = NULL,
  Overwrite = FALSE,
  PreviewOnly = FALSE,
  MaxDownloads = 10,
  Sheets = NULL,
  PageUrl = "https://www.qualityinfo.org/uesti",
  StartYear = NULL,
  EndYear = NULL,
  CacheMaxAgeDays = 1,
  ThrottleSeconds = 0.25,
  MetadataCacheDays = 7,
  RefreshMetadata = FALSE,
  AreaLookup = NULL,
  SeasonalAdjustment = "seasonally adjusted",
  Frequency = "monthly",
  Measures = c("unemployment_rate", "labor_force", "unemployed", "employed"),
  metadata = FALSE,
  Refresh = c("auto", "always", "never"),
  MaxAge = NULL
)
}
\arguments{
\item{Geographies}{Optional QualityInfo geography names or area codes. If
omitted, the selected-area request includes all published areas. Use
\code{OED_LAUS_Options()} to inspect current values.}
\item{Paths}{Optional previously downloaded LAUS workbook paths.}
\item{Urls}{Optional direct workbook URLs.}
\item{DownloadDir}{Directory for downloaded workbooks.}
\item{Overwrite}{Whether existing workbooks may be replaced.}
\item{Refresh}{Cache policy: \code{"auto"} refreshes rolling LAUS workbooks
after one day and uses source/catalog checks when available; \code{"always"}
downloads fresh workbooks; and \code{"never"} reuses valid local workbooks
without a freshness probe. \code{Overwrite = TRUE} is retained as an alias
for \code{Refresh = "always"}.}
\item{MaxAge}{Optional maximum cache age in days overriding the one-day LAUS
default.}
\item{PreviewOnly}{Return a download plan without downloading.}
\item{MaxDownloads}{Maximum number of uncached workbook downloads allowed.}
\item{Sheets}{Optional workbook sheet names.}
\item{PageUrl}{QualityInfo unemployment statistics page.}
\item{StartYear}{Optional first year for the live report. Provide together
with EndYear; if both are omitted, 2000 is used.}
\item{EndYear}{Optional last year for the live report. Provide together
with StartYear. If both years are omitted, the latest year in the bundled
QualityInfo release metadata is selected without a lookup request.}
\item{CacheMaxAgeDays}{Maximum age of a rolling monthly workbook cache before
it is refreshed.}
\item{ThrottleSeconds}{Minimum pause between workbook downloads that actually
reach QualityInfo. Cached files do not trigger a pause.}
\item{MetadataCacheDays}{Maximum age of dynamically discovered page,
geography, and year metadata when a custom page or explicit refresh is used.}
\item{RefreshMetadata}{Whether to opt into dynamic supporting-metadata
discovery. This is not needed for the standard bundled endpoint.}
\item{AreaLookup}{Optional two-column data frame with QualityInfo geography
names in \code{name} and area codes in \code{code}. This is useful when reading
manually downloaded workbooks that do not have a loader-created metadata
sidecar.}
\item{SeasonalAdjustment}{Whether to return seasonally adjusted data, not
seasonally adjusted data, or both.}
\item{Frequency}{Whether to return monthly data, annual averages, or both.}
\item{Measures}{Measures to include. Accepted values are
\code{unemployment_rate}, \code{labor_force}, \code{unemployed}, and
\code{employed}; short names such as \code{ur} and \code{lf} are also
accepted.}
\item{metadata}{Whether to retain date, frequency, workbook, and sheet
metadata in addition to the compact analysis columns.}
}
\details{
The standard \code{/uesti} request uses bundled endpoint, area-code, and year
metadata, so ordinary preview, cache, and download planning makes no
preliminary HTTP requests. The selected-area endpoint still requires one
report request per measure/adjustment combination. A custom \code{PageUrl} or
\code{RefreshMetadata = TRUE} opts into the legacy metadata discovery path.

When \code{DownloadDir} is omitted, LAUS workbooks are stored in the shared
\code{OEDloadR data/LAUS} folder. The root defaults to
\code{file.path(getwd(), "OEDloadR data")} and can be overridden with
\code{options(OEDloadR.data_dir = "D:/custom/path")} or the
\code{OEDLOADR_DATA_DIR} environment variable. Each downloaded workbook has a
JSON metadata sidecar; the existing internal RDS sidecar may also be used to
recover the selected-area lookup for manually downloaded workbooks.

Automatic freshness cannot guarantee that a workbook is current when
QualityInfo supplies neither a server revision header nor a newer-period
signal, so the age policy remains the fallback.
}
\value{
A compact data frame with one row per geography and month by default. The
default columns are year, month, area_code, geography, sa_ur, sa_lf,
sa_unemployed, and sa_employed. The default request uses the selected-area
QualityInfo endpoint for all published areas and retrieves seasonally adjusted
monthly data from January 2000 through the latest available year. Use
\code{SeasonalAdjustment = "both"} to include not seasonally adjusted data,
\code{Frequency = "annual"} or \code{Frequency = "both"} to include annual
averages, and \code{metadata = TRUE} to retain date, frequency, workbook, and
sheet fields. The selected-area endpoint supplies the four monthly measures
separately, so the default plan contains one request per measure. Annual
series use the \code{annual_} prefix. Downloaded results carry
\code{download_plan}, \code{download_diagnostics}, and
\code{laus_diagnostics} attributes.
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
