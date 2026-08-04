# OEDloadR

OEDloadR downloads, reads, and standardizes Oregon Employment Department Workforce and Economic Research Division data
from [QualityInfo.org](https://www.qualityinfo.org/).

# Basic Usage

Commands:

- `OED_QCEW_Table()` — Quarterly Census of Employment and Wages (QCEW) workbooks from QualityInfo.org/ewind
- `OED_LAUS_Table()` — Local Area Unemployment Statistics (CES) workbooks from QualityInfo.org/uesti
- `OED_IProfile()` — Industry Profile workbooks from QualityInfo.org/lipro
- `OED_Businesses()` — Business List workbooks from QualityInfo.org/blist
- `OED_Data()` — Variety of workbooks from QualityInfo.org/data


## Quick start

```r
library(OEDloadR)

# The OED_QCEW_Table() command defaults to pulling the latest annual statewide QCEW data.
qcew <- OED_QCEW_Table()

# The OED_LAUS_Table() command defaults to pulling the latest year's monthly statewide LAUS data. (Note: currently plan to change the defaults to either the entire monthly history of Oregon from Jan 2000 to latest month including all four LAUS variables as well as the annual average data, or to defaulting to the entire January 2000 to latest month data series for all geographies)
laus <- OED_LAUS_Table()

# Select a category and Oregon region from the public /data catalog. (Note: OED_Data() is currently very buggy, in its development stage)
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

`OED_QCEW_Table(PreviewOnly = TRUE)` and the other loaders' `PreviewOnly`
option return a plan without downloading. Existing valid workbooks are reused
unless `Overwrite = TRUE`.

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
and failed files. Use `OED_Diagnostics(result)` for the summary without
accessing attributes directly. QCEW results also retain `failed_requests` and
`naics_availability` attributes when applicable.
