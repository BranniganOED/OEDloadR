# Bundled QualityInfo endpoints and stable lookup tables.
#
# These values describe the public QualityInfo report contracts used by the
# loaders. Keeping them in the package prevents ordinary calls from loading a
# webpage or lookup service merely to rediscover information that changes
# infrequently. If QualityInfo changes one of these contracts, the loader
# fails with an actionable message and the package can be updated deliberately.

oed_static_laus_config <- function(PageUrl = "https://www.qualityinfo.org/uesti") {
  if (!identical(as.character(PageUrl)[1], "https://www.qualityinfo.org/uesti")) {
    return(NULL)
  }

  list(
    page_url = PageUrl,
    report_xlsx_url = paste0(
      "https://www.qualityinfo.org/uesti?",
      "p_p_id=QiDatatoolLabforce_INSTANCE_cTG7oKCBJ915&",
      "p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&",
      "p_p_resource_id=getReportXlsx&p_p_cacheability=cacheLevelPage"
    ),
    service_url = "https://www.qualityinfo.org/lmiservice/service",
    static = TRUE,
    years = seq.int(2000L, 2026L)
  )
}

oed_static_iprofile_endpoint <- function() {
  paste0(
    "https://www.qualityinfo.org/lipro?",
    "p_p_id=QiDatatoolLip_INSTANCE_RRXsaoKvrlto&",
    "p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&",
    "p_p_resource_id=getReportXlsx&p_p_cacheability=cacheLevelPage"
  )
}

oed_static_businesses_endpoint <- function() {
  paste0(
    "https://www.qualityinfo.org/blist?",
    "p_p_id=QiDatatoolIndlookup_INSTANCE_uJIOLXG77d3x&",
    "p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&",
    "p_p_resource_id=getReportXlsx&p_p_cacheability=cacheLevelPage"
  )
}

oed_static_laus_geographies <- function() {
  tibble::tribble(
    ~name, ~code,
    "United States", "0000000000",
    "Oregon", "4101000000",
    "Albany, OR MSA", "4121010540",
    "Bend, OR MSA", "4121013460",
    "Corvallis, OR MSA", "4121018700",
    "Eugene-Springfield, OR MSA", "4121021660",
    "Grants Pass, OR MSA", "4121024420",
    "Medford, OR MSA", "4121032780",
    "Portland-Vancouver-Hillsboro, OR-WA MSA", "4121038900",
    "Salem, OR MSA", "4121041420",
    "Baker County", "4104000001",
    "Benton County", "4104000003",
    "Clackamas County", "4104000005",
    "Clatsop County", "4104000007",
    "Columbia County", "4104000009",
    "Coos County", "4104000011",
    "Crook County", "4104000013",
    "Curry County", "4104000015",
    "Deschutes County", "4104000017",
    "Douglas County", "4104000019",
    "Gilliam County", "4104000021",
    "Grant County", "4104000023",
    "Harney County", "4104000025",
    "Hood River County", "4104000027",
    "Jackson County", "4104000029",
    "Jefferson County", "4104000031",
    "Josephine County", "4104000033",
    "Klamath County", "4104000035",
    "Lake County", "4104000037",
    "Lane County", "4104000039",
    "Lincoln County", "4104000041",
    "Linn County", "4104000043",
    "Malheur County", "4104000045",
    "Marion County", "4104000047",
    "Morrow County", "4104000049",
    "Multnomah County", "4104000051",
    "Polk County", "4104000053",
    "Sherman County", "4104000055",
    "Tillamook County", "4104000057",
    "Umatilla County", "4104000059",
    "Union County", "4104000061",
    "Wallowa County", "4104000063",
    "Wasco County", "4104000065",
    "Washington County", "4104000067",
    "Wheeler County", "4104000069",
    "Yamhill County", "4104000071"
  ) |>
    dplyr::mutate(name_key = laus_live_key(.data$name))
}

oed_static_qcew_geographies <- function() {
  tibble::tribble(
    ~geo_type, ~area_type, ~geo_code, ~qcew_area, ~geography, ~geography_short,
    "State", "01", "000000", "4101000000", "Oregon", "Oregon",
    "MSA", "56", "006441", "4156006441", "Portland-Vancouver-Hillsboro OR-WA MSA, Oregon Portion", "Portland-Vancouver-Hillsboro OR-WA MSA, Oregon Portion",
    "MSA", "02", "007080", "4102007080", "Salem MSA", "Salem MSA",
    "County", "04", "000001", "4104000001", "Baker County", "Baker",
    "County", "04", "000003", "4104000003", "Benton County", "Benton",
    "County", "04", "000005", "4104000005", "Clackamas County", "Clackamas",
    "County", "04", "000007", "4104000007", "Clatsop County", "Clatsop",
    "County", "04", "000009", "4104000009", "Columbia County", "Columbia",
    "County", "04", "000011", "4104000011", "Coos County", "Coos",
    "County", "04", "000013", "4104000013", "Crook County", "Crook",
    "County", "04", "000015", "4104000015", "Curry County", "Curry",
    "County", "04", "000017", "4104000017", "Deschutes County", "Deschutes",
    "County", "04", "000019", "4104000019", "Douglas County", "Douglas",
    "County", "04", "000021", "4104000021", "Gilliam County", "Gilliam",
    "County", "04", "000023", "4104000023", "Grant County", "Grant",
    "County", "04", "000025", "4104000025", "Harney County", "Harney",
    "County", "04", "000027", "4104000027", "Hood River County", "Hood River",
    "County", "04", "000029", "4104000029", "Jackson County", "Jackson",
    "County", "04", "000031", "4104000031", "Jefferson County", "Jefferson",
    "County", "04", "000033", "4104000033", "Josephine County", "Josephine",
    "County", "04", "000035", "4104000035", "Klamath County", "Klamath",
    "County", "04", "000037", "4104000037", "Lake County", "Lake",
    "County", "04", "000039", "4104000039", "Lane County", "Lane",
    "County", "04", "000041", "4104000041", "Lincoln County", "Lincoln",
    "County", "04", "000043", "4104000043", "Linn County", "Linn",
    "County", "04", "000045", "4104000045", "Malheur County", "Malheur",
    "County", "04", "000047", "4104000047", "Marion County", "Marion",
    "County", "04", "000049", "4104000049", "Morrow County", "Morrow",
    "County", "04", "000051", "4104000051", "Multnomah County", "Multnomah",
    "County", "04", "000053", "4104000053", "Polk County", "Polk",
    "County", "04", "000055", "4104000055", "Sherman County", "Sherman",
    "County", "04", "000057", "4104000057", "Tillamook County", "Tillamook",
    "County", "04", "000059", "4104000059", "Umatilla County", "Umatilla",
    "County", "04", "000061", "4104000061", "Union County", "Union",
    "County", "04", "000063", "4104000063", "Wallowa County", "Wallowa",
    "County", "04", "000065", "4104000065", "Wasco County", "Wasco",
    "County", "04", "000067", "4104000067", "Washington County", "Washington",
    "County", "04", "000069", "4104000069", "Wheeler County", "Wheeler",
    "County", "04", "000071", "4104000071", "Yamhill County", "Yamhill"
  )
}

oed_static_qcew_years <- function() seq.int(2000L, 2026L)

oed_static_qcew_latest_year <- function() 2026L

oed_static_data_urls <- function() {
  tibble::tribble(
    ~file_title, ~download_url,
    "Clackamas Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Clackamas Occupational Wage Information/f956d614-5649-54ba-373d-49099729fc94?version=2.0",
    "East Cascades High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/East Cascades High-Wage, High-Demand, High-Skill Occupations 2024-2034/c2690ccd-8fb3-a940-6912-24d0d825e130?version=1.0",
    "East Cascades Industry Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/East Cascades Industry Employment Projections 2024-2034/bd9fbb8a-dcdd-6849-d0af-de76e3ebbb11?version=1.0",
    "East Cascades Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/East Cascades Occupational Employment Projections 2024-2034/d7370b15-5457-825d-b925-e9c1f663ed67?version=1.0",
    "East Cascades Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/East Cascades Occupational Wage Information/e1a0057b-f5d9-94e3-719c-46c3eaa72daa?version=2.0",
    "East Cascades Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/East Cascades Occupations in Demand 2024-2034/efc846e2-da0c-7192-6023-cd43ad38c527?version=1.1",
    "East Cascades STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/East Cascades STEM Employment Projections and Wages by Detailed Occupation 2024-2034/e5861367-125b-7c1e-8f7e-93f812327ab0?version=2.0",
    "Eastern Oregon High-Demand, High-Wage, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Eastern Oregon High-Demand, High-Wage, High-Skill Occupations 2024-2034/8a33c027-c037-dcac-584a-65522661db77?version=1.0",
    "Eastern Oregon Industry Employment Projections, 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Eastern Oregon Industry Employment Projections, 2024-2034/a814c350-587e-bf98-2184-dcacf7bad863?version=1.0",
    "Eastern Oregon Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Eastern Oregon Occupational Employment Projections 2024-2034/40dfa669-0e3f-aadc-a4f9-4cfb526de33e?version=1.0",
    "Eastern Oregon Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Eastern Oregon Occupational Wage Information/03de3564-3da7-173b-0dff-62d3577f5706?version=2.0",
    "Eastern Oregon Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Eastern Oregon Occupations in Demand 2024-2034/1cf57e54-2e5c-804c-3c7f-6eb6e11babad?version=1.1",
    "Eastern Oregon STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Eastern Oregon STEM Employment Projections and Wages by Detailed Occupation 2024-2034/cd39aa14-61ca-b552-a10a-e9c1e886a888?version=2.0",
    "Lane High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Lane High-Wage, High-Demand, High-Skill Occupations 2024-2034/41374bf5-1053-2de8-e11d-0a83669574da?version=1.0",
    "Lane Industry Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Lane Industry Employment Projections 2024-2034/9a77b087-0b69-1fe5-d256-5aa380f1865c?version=1.0",
    "Lane Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Lane Occupational Employment Projections 2024-2034/5ee79f6e-dbc4-7bb5-e0d5-c066f5f5bcb8?version=1.0",
    "Lane Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Lane Occupational Wage Information/9a273b31-b7de-9353-84a4-b808ba28a701?version=2.0",
    "Lane Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Lane Occupations in Demand 2024-2034/98148c60-3d71-7579-af3d-2705545fc346?version=1.1",
    "Lane STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Lane STEM Employment Projections and Wages by Detailed Occupation 2024-2034/4014bff9-acd4-4452-6b1f-5ee1e204a348?version=2.0",
    "Mid-Valley High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Mid-Valley High-Wage, High-Demand, High-Skill Occupations 2024-2034/a3502771-beb2-e248-5c68-7f2a392774f6?version=1.0",
    "Mid-Valley Industry Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Mid-Valley Industry Employment Projections 2024-2034/2f18ce73-927f-bb10-be5c-c3958d162c48?version=1.0",
    "Mid-Valley Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Mid-Valley Occupational Employment Projections 2024-2034/577bed62-b2a5-d57d-c6ee-58e0c4281e82?version=1.0",
    "Mid-Valley Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Mid-Valley Occupational Wage Information/e0711d8b-ab5b-045a-6b36-47bde7aa84a8?version=2.0",
    "Mid-Valley Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Mid-Valley Occupations in Demand 2024-2034/993ed011-560e-f8a5-034c-d33cb7397ec7?version=1.1",
    "Mid-Valley STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Mid-Valley STEM Employment Projections and Wages by Detailed Occupation 2024-2034/34186366-6fa4-0b51-6276-370cdd8444c2?version=2.0",
    "Northwest Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Northwest Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034/30b58c61-507a-9588-2f8c-a4f06a3114d2?version=1.0",
    "Northwest Oregon Industry Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Northwest Oregon Industry Employment Projections 2024-2034/3221a943-d6ce-b0a8-96ae-83f1a1a13c47?version=1.0",
    "Northwest Oregon Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Northwest Oregon Occupational Employment Projections 2024-2034/f4f47f49-f82f-817d-f493-ba9acb06984f?version=1.0",
    "Northwest Oregon Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Northwest Oregon Occupational Wage Information/345f8f68-e688-f91b-0506-ac21c5ef0894?version=2.0",
    "Northwest Oregon Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Northwest Oregon Occupations in Demand 2024-2034/bed44f5d-f64f-bf44-6ccc-52d6047ae47a?version=1.1",
    "Northwest STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Northwest STEM Employment Projections and Wages by Detailed Occupation 2024-2034/02e54610-8a20-9d5d-6700-8ccfcc88fa72?version=2.0",
    "Oregon Agricultural Employment", "https://www.qualityinfo.org/documents/20118/37545/Oregon Agricultural Employment/cdb9d43d-651a-fd71-a5f0-eeda20b415a3?version=2.0",
    "Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034/27d1f35d-cea7-1ef0-47bb-c6b5b5e22bec?version=1.0",
    "Oregon Industry Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Oregon Industry Employment Projections 2024-2034/772128d9-c752-966a-e0f3-37f35420c39d?version=1.3",
    "Oregon Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Oregon Occupational Employment Projections 2024-2034/2ffe58de-633b-4158-f4de-566322b093be?version=1.3",
    "Oregon Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Oregon Occupational Wage Information/1b7c0fb8-10b1-94d7-d59c-99f244926c50?version=2.0",
    "Oregon Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Oregon Occupations in Demand 2024-2034/c25c9a9a-fc51-debc-76a4-87cd4dca087a?version=1.1",
    "Oregon STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Oregon STEM Employment Projections and Wages by Detailed Occupation 2024-2034/dce92715-2322-1dc9-27de-ab37dc9fc366?version=2.0",
    "Portland Tri-County High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Portland Tri-County High-Wage, High-Demand, High-Skill Occupations 2024-2034/9b798187-6bbb-e1b3-7282-4c7c6e65e713?version=1.0",
    "Portland Tri-County Industry Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Portland Tri-County Industry Projections 2024-2034/8dd44f95-dd35-5255-0e17-86b562938864?version=1.0",
    "Portland Tri-County Occupational Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Portland Tri-County Occupational Projections 2024-2034/d9d47cad-8384-b14e-f61f-7a6f34e45902?version=1.0",
    "Portland Tri-County Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Portland Tri-County Occupations in Demand 2024-2034/9c7962b7-a859-4fd8-db4a-f47390fe625b?version=1.1",
    "Portland Tri-County STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Portland Tri-County STEM Employment Projections and Wages by Detailed Occupation 2024-2034/f240e7be-e96a-24cb-6d1c-7afd966b43d5?version=2.0",
    "Portland-Metro Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Portland-Metro Occupational Wage Information/ac32b9e7-a57b-5f68-98c4-3d78f5e0d4f6?version=2.0",
    "Rogue Valley High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Rogue Valley High-Wage, High-Demand, High-Skill Occupations 2024-2034/3f38a074-4d52-ca86-7a7f-76253ec5589e?version=1.0",
    "Rogue Valley Industry Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Rogue Valley Industry Employment Projections 2024-2034/c51fb88b-1c65-2bad-80f4-53aefeba5e60?version=1.0",
    "Rogue Valley Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Rogue Valley Occupational Employment Projections 2024-2034/cd3576c4-bf12-5581-19cc-6f38123eaf1d?version=1.0",
    "Rogue Valley Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Rogue Valley Occupational Wage Information/61c1c72a-7f84-0a84-709c-70ae36f55cfa?version=2.0",
    "Rogue Valley Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Rogue Valley Occupations in Demand 2024-2034/a26433e8-80e9-7679-3a91-ea350f11fdbe?version=1.1",
    "Rogue Valley STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Rogue Valley STEM Employment Projections and Wages by Detailed Occupation 2024-2034/fdbddd8b-d4fb-b476-39e4-41c1e0168aad?version=2.0",
    "Southwestern Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034", "https://www.qualityinfo.org/documents/20118/37539/Southwestern Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034/defcf0f6-f2d4-331c-7595-1562bbdb9ecb?version=1.0",
    "Southwestern Oregon Industry Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Southwestern Oregon Industry Employment Projections 2024-2034/2f9102bf-7a83-c67b-3274-b3e9112f34c4?version=1.0",
    "Southwestern Oregon Occupational Employment Projections 2024-2034", "https://www.qualityinfo.org/documents/20118/37537/Southwestern Oregon Occupational Employment Projections 2024-2034/e731d89f-3af2-f245-7d83-03706c8411c0?version=1.0",
    "Southwestern Oregon Occupational Wage Information", "https://www.qualityinfo.org/documents/20118/37549/Southwestern Oregon Occupational Wage Information/533fe4b0-0966-37e0-4a07-4c8e276fd191?version=2.0",
    "Southwestern Oregon Occupations in Demand 2024-2034", "https://www.qualityinfo.org/documents/20118/37543/Southwestern Oregon Occupations in Demand 2024-2034/a493600c-ad13-ef19-cd8d-fac4d4a7cfb9?version=1.1",
    "Southwestern Oregon STEM Employment Projections and Wages by Detailed Occupation 2024-2034", "https://www.qualityinfo.org/documents/20118/37565/Southwestern Oregon STEM Employment Projections and Wages by Detailed Occupation 2024-2034/f3fbd26b-4d66-953a-1267-83254d32826c?version=2.0"
  ) |>
    dplyr::mutate(file_title_key = data_title_key(.data$file_title))
}
