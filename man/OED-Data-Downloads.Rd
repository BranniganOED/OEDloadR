% OED_Data, OED_IProfile, and OED_Businesses documentation

\name{OED_Data}
\alias{OED_Data}
\alias{OED_IProfile}
\alias{OED_Businesses}
\title{Download and Read QualityInfo Excel Workbooks}

\description{
Discover, download, and read workbook data from QualityInfo's public data,
Industry Profile, and Business List pages. The shared workflow supports
local paths, explicit URLs, preview-only plans, generic sheet cleaning, and
safe reuse of existing workbooks.
}

\usage{
OED_Data(
  Category = NULL,
  Geographies = NULL,
  DownloadDir = file.path("output", "qualityinfo_data"),
  Overwrite = FALSE,
  Read = TRUE,
  Clean = TRUE,
  PreviewOnly = FALSE,
  List = FALSE,
  MaxDownloads = 10,
  Paths = NULL,
  Sheets = NULL,
  DataUrl = "https://www.qualityinfo.org/data"
)

OED_IProfile(
  Paths = NULL,
  Urls = NULL,
  DownloadDir = file.path("output", "qualityinfo_iprofile"),
  Overwrite = FALSE,
  Read = TRUE,
  Clean = TRUE,
  PreviewOnly = FALSE,
  MaxDownloads = 10,
  Sheets = NULL,
  PageUrl = "https://www.qualityinfo.org/lipro"
)

OED_Businesses(
  Paths = NULL,
  Urls = NULL,
  DownloadDir = file.path("output", "qualityinfo_businesses"),
  Overwrite = FALSE,
  Read = TRUE,
  Clean = TRUE,
  PreviewOnly = FALSE,
  MaxDownloads = 10,
  Sheets = NULL,
  PageUrl = "https://www.qualityinfo.org/blist"
)
}
\arguments{
\item{Category}{Optional data-page category name or alias. A bare
\code{OED_Data()} call defaults to Oregon industry employment projections.}
\item{Geographies}{Optional data-page geography name or alias. A bare
\code{OED_Data()} call defaults to Oregon.}
\item{DownloadDir}{Directory for downloaded workbooks.}
\item{Overwrite}{Whether existing workbooks may be replaced.}
\item{Read}{Whether downloaded workbooks should be read.}
\item{Clean}{Whether workbook sheets should receive generic cleaning.}
\item{PreviewOnly}{Return a download plan without downloading.}
\item{List}{For \code{OED_Data()}, return the built-in catalog.}
\item{MaxDownloads}{Maximum number of downloads allowed.}
\item{Paths}{Optional previously downloaded workbook paths. When omitted,
the function uses the QualityInfo web source.}
\item{Sheets}{Optional workbook sheet names.}
\item{DataUrl}{QualityInfo data-page URL.}
\item{Urls}{Optional direct workbook URLs.}
\item{PageUrl}{QualityInfo page used for discovery.}
}
\value{
A data frame containing a catalog, download plan, or combined downloaded
workbook data depending on the arguments. Downloaded results carry
\code{download_plan} and \code{download_diagnostics} attributes.
}
\details{
When web download mode is used, the Industry Profile and Business List
wrappers use visible direct workbook links or explicit \code{Urls}; they do
not guess undocumented interactive request parameters.
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
OED_IProfile(PreviewOnly = TRUE)
OED_Businesses(PreviewOnly = TRUE)
}
}
