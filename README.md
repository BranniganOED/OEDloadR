# OEDloadR

OEDloadR downloads, reads, and standardizes Oregon Employment Department data
from [QualityInfo.org](https://www.qualityinfo.org/). It uses QualityInfo's
public services and Excel workbooks, including Oregon statewide, county, and
Oregon-region data.

The command families are designed to be predictable for interactive use:

- `OED_QCEW_Table()` — Quarterly Census of Employment and Wages
- `OED_LAUS_Table()` — Local Area Unemployment Statistics
- `OED_Data()` — public QualityInfo workbook catalog
- `OED_IProfile()` — Industry Profile workbooks
- `OED_Businesses()` — Business List workbooks

## Quick start

```r
library(OEDloadR)

# The default is a small Oregon statewide QCEW request.
qcew <- OED_QCEW_Table()

laus <- OED_LAUS_Table(
  Geographies = "Washington County"
)

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

## QualityInfo-specific behavior

OEDloadR keeps QualityInfo's backend and data model: live portlet endpoints,
QualityInfo service lookups, Excel workbook parsing, Oregon geography names and
codes, suppression markers, provisional flags, and Oregon-specific workbook
structures. It does not require BLS series IDs or BLS flat files.

