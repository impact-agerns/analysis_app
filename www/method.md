## Methodology

There are 3 innovative analysis methodologies available in this analysis app.

The Aggregation & Analysis page allows to conduct two steps based on clean structured community KI (AoK) data: 1) the aggregation of data from multiple KIs reporting on the same unit of analysis (e.g. settlement); 2) the analysis of (aggregated) data at the area (e.g. admin 2) level.

The Severity Index methodology offers a standardized approach to summarise AoK data to identify severity of acute needs.

### 1. Data aggregation

Structured community KI (AoK) data can involve multiple KIs reporting on the same communities (e.g. settlements). For example, 3 KIs might be interviewed about the same settlement, while for another settlement 2 KIs might be interviewed. However, KIs might disagree with each other in answering the same question about the same settlement.

Before aggregation, each line in the (clean) data essentially means "In settlement A, KI 1 reported" and "In settlement A, KI 2 reported", etc. (i.e. there are several lines per settlement if there are several KIs per settlement).

An aggregation approach is thus used to identify consensus (or lack thereof) between KIs reporting on the same settlement, to obtain a single triangulated response for each assessed settlement. Another reason to do this is that different numbers of KIs interviewed in different assessed settlement would also introduce bias towards settlements with more KIs if results are not aggregated at settlement-level before comparisons are made between assessed settlements and areas.

When only 1 KI is interviewed in each assessed community (e.g. settlement), there is no need for this first step of aggregation to the unit of analysis (e.g. settlement) level (as there is already only 1 line per settlement in the clean data).

Aggregation rules: Depending on the type of indicator the data for each indicator is aggregated as follows:

`select_one`: Mode (Most common choice reported by KIs)

`select_multiple`: All choices reported by all KIs are kept

`integer`: Mean (arithmetic)

`text`: Mode

After aggregating the data at the level of the unit of analysis (e.g. settlement), the dataset then shows 1 row for each unit (e.g. settlement) instead of previously being 1 row per KI. Each row then essentially means "In settlement A, KIs (through consensus aggregation) reported...".\

### 2. Data analysis

Structured community KI (AoK) data aggregated at the level of the unit of analysis (e.g. settlement) produced in step 1 can already be mapped and reported in a useful way (e.g. settlement-level mapping of results can very clearly show geographic patterns of need). However, a comparison of different areas (e.g. admin 2) is often necessary to inform the prioritisation of humanitarian responses between these areas.

For this reason, community (e.g. settlement) level data can be summarised at the area level (e.g. admin 2 or 1, or even livelihood zones) through this data analysis step. This simply entails descriptive statistics in the form of frequency tables for each area, showing the total number of assessed communities (e.g. settlements) in which KIs were asked a question, and the number of times each response choice was reported by KIs, along with percentages. For example, "in 40% of assessed settlements in District A, KIs reported that"

Again, step 1 is not necessary when only 1 KI is interviewed in each assessed community (e.g. settlement). In such cases, step 2 can be directly used to produce the analysis. (In some very rare cases, there might be a desire to skip step 1 even when the data has multiple KIs per community (e.g. settlement), so that KI disagreements may be visible in the results for each community " however this is not recommended as it has only limited value and only in very specific cases, and makes the results incomparable at area level because results would be biased by varying numbers of KI interviewed in different communities).

The Data analysis step in the app also optionally allows to disaggregate area-level results by a second chosen indicator. In this case the analysis script will set the 2 selected variables as dimensions of the analysis and identify for each distinct combination of the 2 variables all response statistics for all indicators/choices. For example, after selecting "admin2" as the (first) level of analysis, an indicator such as KI gender could also be added. Results would then be presented by area, disaggregated further by KI gender.

### 3. Severity Index

The AoK Severity Index (AoK SI) is a composite index methodology that aims to provide a standardized and efficient approach to summarising and interpreting AoK data into meaningful & actionable analysis of acute multisectoral humanitarian needs at community (e.g. settlement) and area (e.g. admin 2) level. It enables the identification of the likely severity of acute needs, the multisectoral and sectoral drivers of need, geographic hotspots of need, and the evolution of needs over time (when used for multiple rounds of monitoring). It thus allows for meaningful comparisons of needs between areas and over time, including across administrative or other boundaries. The AoK SI can be used for any assessments using the AoK methodology, including single assessments (e.g. rapid needs assessments) and recurring monitoring assessments (e.g. HSM).

The AoK SI is based on the IMPACT Acute Needs Analytical Framework (ANF) and other sectoral analytical frameworks of reference (see details below), which underpin the IMPACT AoK Indicator Bank (see section 3.1 for more details).

The detailed Terms of Reference (under review) for the AoK Severity Index can be found here: [TOR](aok_severity_index_tor_Feb2025_shareable.pdf)

Github repository to access scripts (<https://github.com/agerns/analysis_app>)
