% OED_Data, OED_IProfile, and OED_Businesses documentation

\name{OED_Data}
\alias{OED_Data}
\alias{OED_IProfile}
\alias{OED_Businesses}
\title{Download and Read QualityInfo Excel Workbooks}

\description{
Select, download, and read workbook data from QualityInfo's public data,
Industry Profile, and Business List products. The standard data catalog is
bundled in the package; the shared workflow supports local paths, explicit
URLs, preview-only plans, generic sheet cleaning, and safe reuse of existing
workbooks.
}

\usage{
OED_Data(
  Category = NULL,
  Geographies = NULL,
  DownloadDir = NULL,
  Overwrite = FALSE,
  Read = TRUE,
  Clean = TRUE,
  PreviewOnly = FALSE,
  List = FALSE,
  MaxDownloads = 10,
  Paths = NULL,
  Sheets = NULL,
  DataUrl = "https://www.qualityinfo.org/data",
  Refresh = c("auto", "always", "never"),
  MaxAge = NULL
)

OED_IProfile(
  Paths = NULL,
  Urls = NULL,
  DownloadDir = NULL,
  Overwrite = FALSE,
  Read = TRUE,
  Clean = TRUE,
  PreviewOnly = FALSE,
  MaxDownloads = 10,
  Sheets = NULL,
  PageUrl = "https://www.qualityinfo.org/lipro",
  Refresh = c("auto", "always", "never"),
  MaxAge = NULL
)

OED_Businesses(
  Paths = NULL,
  Urls = NULL,
  DownloadDir = NULL,
  Overwrite = FALSE,
  Read = TRUE,
  Clean = TRUE,
  PreviewOnly = FALSE,
  MaxDownloads = 10,
  Sheets = NULL,
  PageUrl = "https://www.qualityinfo.org/blist",
  Refresh = c("auto", "always", "never"),
  MaxAge = NULL
)
}
\arguments{
\item{Category}{Optional data-page category name or alias. A bare
\code{OED_Data()} call defaults to Oregon industry employment projections.}
\item{Geographies}{Optional data-page geography name or alias. A bare
\code{OED_Data()} call defaults to Oregon.}
\item{DownloadDir}{Directory for downloaded workbooks. When omitted, the
shared dataset-specific folder is used.}
\item{Overwrite}{Whether existing workbooks may be replaced.}
\item{Refresh}{Cache policy: \code{"auto"} applies dataset-specific freshness
rules, \code{"always"} downloads a fresh workbook, and \code{"never"}
reuses a valid local workbook without a freshness probe. \code{Overwrite = TRUE}
is retained as an alias for \code{Refresh = "always"}.}
\item{MaxAge}{Optional maximum cache age in days. When \code{NULL}, the
dataset-specific default is used.}
\item{Read}{Whether downloaded workbooks should be read.}
\item{Clean}{Whether workbook sheets should receive generic cleaning.}
\item{PreviewOnly}{Return a download plan without downloading.}
\item{List}{For \code{OED_Data()}, return the built-in catalog.}
\item{MaxDownloads}{Maximum number of downloads allowed.}
\item{Paths}{Optional previously downloaded workbook paths. When omitted,
\code{OED_Data()} uses the bundled catalog; the interactive report wrappers
require explicit \code{Urls}.}
\item{Sheets}{Optional workbook sheet names.}
\item{DataUrl}{QualityInfo data-page URL. The default uses bundled document
URLs; a custom URL enables the legacy page-link discovery path.}
\item{Urls}{Optional direct workbook URLs.}
\item{PageUrl}{QualityInfo interactive report page. Supply \code{Urls} for
\code{OED_IProfile()} and \code{OED_Businesses()}; hidden request options are
not rediscovered automatically.}
}
\value{
A data frame containing a catalog, download plan, or combined downloaded
workbook data depending on the arguments. Downloaded results carry
\code{download_plan} and \code{download_diagnostics} attributes.
}
\details{
When web download mode is used, \code{OED_Data()} uses bundled document IDs
and makes one request per selected workbook. The Industry Profile and Business
List wrappers require explicit \code{Urls} because their pages use interactive
request parameters that are not safely reproducible from stable page links.
They do not load the page merely to rediscover those parameters.

When \code{DownloadDir} is omitted, workbooks are stored under the shared
\code{OEDloadR data} root. The root is
\code{file.path(getwd(), "OEDloadR data")} by default and can be changed with
\code{options(OEDloadR.data_dir = "D:/custom/path")} or the
\code{OEDLOADR_DATA_DIR} environment variable. The Data, Industry Profiles,
and Businesses commands use their own subfolders. Each downloaded workbook
has a JSON metadata sidecar recording the request and download details.

Automatic freshness uses catalog changes, server revision headers when
available, and age-based fallbacks. A source that does not expose revision
metadata cannot be declared current beyond these checks.
}
\examples{
\dontrun{
library(OEDloadR)
OED_Data(
  Category = "Industry Employment Projections",
  Geographies = "Oregon",
  PreviewOnly = TRUE
)

OED_Data(List = TRUE)
OED_IProfile(Urls = c(example = "https://example.org/example.xlsx"),
            PreviewOnly = TRUE)
OED_Businesses(Urls = c(example = "https://example.org/example.xlsx"),
               PreviewOnly = TRUE)
}
}
