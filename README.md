# OEDloadR

OEDloadR downloads, reads, and standardizes Oregon Employment Department Workforce and Economic Research Division data
from [QualityInfo.org](https://www.qualityinfo.org/).

# Basic Usage

Commands:

- `OED_QCEW_Table()` — Quarterly Census of Employment and Wages (QCEW) workbooks from [QualityInfo.org/ewind](https://qualityinfo.org/web/guest/ewind)
- `OED_LAUS_Table()` — Local Area Unemployment Statistics (LAUS) workbooks from [QualityInfo.org/uesti](https://qualityinfo.org/web/guest/uesti)
- `OED_IProfile()` — Industry Profile workbooks from [QualityInfo.org/lipro](https://qualityinfo.org/web/guest/lipro)
- `OED_Businesses()` — Business List workbooks from [QualityInfo.org/blist](https://qualityinfo.org/web/guest/blist)
- `OED_Data()` — Variety of workbooks from [QualityInfo.org/data](https://qualityinfo.org/data)


## Quick start

```r
library(OEDloadR)

# The OED_QCEW_Table() command defaults to pulling the latest annual statewide QCEW data.
qcew <- OED_QCEW_Table()

# The bare OED_LAUS_Table() call downloads seasonally adjusted monthly data
# for all published areas from January 2000 through the latest available month.
# It returns one compact row per area/month with four measures: sa_ur, sa_lf,
# sa_unemployed, and sa_employed. Use SeasonalAdjustment = "both" or
# Frequency = "both" when the unadjusted or annual series are needed.
# QualityInfo supplies the four monthly measures separately through the
# selected-area endpoint, so the default plan contains four workbook requests.
laus <- OED_LAUS_Table()

# Select a category and Oregon region from the public /data catalog.
projections <- OED_Data(
  Category = "industry projections",
  Geographies = "Portland Tri-County"
)
```

## Discover choices before downloading

Use the catalog and live option helpers instead of guessing labels or codes:

```r
data_catalog <- OED_Data(List = TRUE)
qcew_options <- OED_QCEW_Options()
laus_options <- OED_LAUS_Options()

head(data_catalog)
head(qcew_options$geographies)
head(laus_options$geographies)
```

The standard catalog and option helpers are bundled with the package, so these
calls are offline. Ordinary uncached pulls go directly to the required
QualityInfo report request; they do not first load a webpage or redownload a
lookup table. Valid cached workbooks, supplied `Paths`, and `PreviewOnly = TRUE`
also make zero HTTP requests.

`OED_IProfile()` and `OED_Businesses()` use interactive QualityInfo report
pages rather than stable direct-link catalogs. Supply explicit workbook `Urls`
or local `Paths`; the package will not load those pages merely to rediscover
hidden form values. `OED_Data(DataUrl = ...)` retains the page-link workflow
when a custom data-page URL is explicitly supplied.

`OED_QCEW_Table(PreviewOnly = TRUE)` and the other loaders' `PreviewOnly`
option return a plan without downloading. By default, downloads are stored in
one shared `OEDloadR data` root with dataset-specific subfolders:

```text
OEDloadR data/
├── QCEW/
├── LAUS/
├── Data/
├── Industry Profiles/
└── Businesses/
```

The default root is `file.path(getwd(), "OEDloadR data")`. Set
`options(OEDloadR.data_dir = "D:/custom/path")` or the
`OEDLOADR_DATA_DIR` environment variable to use another root. An explicit
`DownloadDir` still takes precedence for backward compatibility; the package
does not silently search the old `output/` folders.

All five loaders accept `Refresh = c("auto", "always", "never")` and
`MaxAge`. `auto` uses dataset-specific age rules—LAUS one day, latest QCEW
seven days, older fixed QCEW periods 90 days, and the other workbook families
30 days—while also using catalog, latest-period, and server revision checks
when available. `always` forces a fresh download and `never` reuses a valid
local workbook without a freshness probe. `Overwrite = TRUE` remains an alias
for `Refresh = "always"`.

Each successfully downloaded workbook is written through a temporary file,
validated as an XLSX, atomically moved into place, and accompanied by a
`.metadata.json` sidecar containing the request, source, timestamp, package
version, hash, and available freshness metadata. If a refresh fails but the
previous workbook is valid, the previous copy is retained and reported as
`stale_cache_used`.

```r
plan <- OED_QCEW_Table(
  Geographies = "Washington",
  Years = 2024,
  Periods = "Annual",
  NAICS = "54",
  NAICSMatch = "prefix",
  PreviewOnly = TRUE
)
```

Downloaded results include a `download_plan` attribute and a
`download_diagnostics` attribute describing requested, downloaded, reused,
refreshed, stale-cache, and failed files, along with cache age, refresh
reasons, and metadata paths. Use `OED_Diagnostics(result)` for the summary
without accessing attributes directly. QCEW results also retain
`failed_requests` and `naics_availability` attributes when applicable.

Freshness has limits: QualityInfo does not always expose ETag or
Last-Modified headers or a machine-readable current period. Revision headers
are recorded when a workbook is downloaded, but the package does not probe
them during a valid cache read. In those cases it uses the bundled
catalog/latest-period checks and dataset-specific age policy; it does not claim
that a workbook is current solely because it is present locally.
