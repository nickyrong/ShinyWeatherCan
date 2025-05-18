# Quantile Mapping Infill for Climate Station Data

This feature enables users to infill missing data in Environment and Climate Change Canada (ECCC) climate station records using a technique called quantile mapping. This approach leverages data from nearby climate stations to estimate missing values while preserving the statistical characteristics of the target station's data.

## Methodology

The quantile mapping technique involves the following steps:

1. **Find Nearby Stations**: The application identifies climate stations within a specified distance that have overlapping periods of record with the target station.

2. **Establish Relationships**: For periods when both stations have data, a statistical relationship is established using empirical cumulative distribution functions (ECDFs).

3. **Apply Quantile Mapping**: For periods when the target station is missing data but the nearby station has data, the application:
   - Determines where the nearby station's value falls in its distribution
   - Maps this quantile to the corresponding value in the target station's distribution

4. **Validation**: The quality of the infill can be assessed by comparing original and infilled data statistics and by examining validation metrics when test data is artificially removed.

## Using the Infill Feature

1. Navigate to the "Infill Data" tab in the application.

2. Select your target station.

3. Choose the climate variable to infill (e.g., mean temperature, precipitation).

4. Set the date range for the infill process.

5. Adjust advanced settings if needed:
   - Maximum distance for nearby stations
   - Minimum required period of overlap
   - Whether to process by month (seasonal adjustments)
   - Minimum required data points for mapping

6. Click "Find Nearby Stations" to identify potential donor stations.

7. Review the nearby stations map and table.

8. Click "Run Infill" to execute the quantile mapping process.

9. Explore the results through interactive visualizations:
   - Time series of original and infilled data
   - Donor station contribution summary
   - Monthly infill statistics
   - Before/after summary statistics

10. Download the infilled data or summary statistics as needed.

## Limitations

- Quantile mapping works best when there is a strong correlation between the target and nearby stations.
- Performance may degrade with increasing distance between stations.
- The method assumes stationarity in the relationship between stations over time.
- Extreme values may be less accurately infilled than values in the middle of the distribution.
- For the best results, use multiple nearby stations to infill different periods.
