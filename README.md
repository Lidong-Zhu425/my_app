# README for 2020 U.S. Election Data Visualization Shiny App\n\n
## Overview\n\n
This Shiny application provides a detailed visualization of the 2020 U.S. Presidential Election data. It combines various data sets related to the election and presents interactive visualizations, including national popular votes, electoral votes by state, and a comparison of Biden and Trump vote shares. The application features multiple tabs that users can navigate through to explore different aspects of the election data.
## Features\n\n
The application contains the following tabs:\n\n
1. **2020 Presidential Election Overview**: This tab presents a pie chart visualizing the distribution of the national popular vote among different candidates. Users can also select a candidate to see more detailed information about their vote count and percentage.\n\n
2. **Presidential Electoral and Popular Vote by State**: This tab features a map of the United States, showing the winner of each state by color (blue for Biden, red for Trump). Users can click on individual states to view additional information about the electoral and popular votes for that state.\n\n
3. **Percentage Difference in R/D Vote Share**: This tab shows a bar chart and an interactive data table that illustrate the percentage difference in vote share between Biden and Trump across all states. The chart highlights whether Biden or Trump won in each state, and users can examine the difference visually and interactively.\n\n
## Data Sources\n\n
The application uses data from the following sources:\n\n
- **`federalelections2020.xlsx`**: Available at https://www.fec.gov/introduction-campaign-finance/election-results-and-voting-information/ \n\n
- **`election_data.csv`**: Available at https://www.fec.gov/introduction-campaign-finance/election-results-and-voting-information/ .\n
## Dependencies\n\n
Make sure the following R packages are installed:\n\n
- shiny\n
- ggplot2\n
- DT\n
- leaflet\n
- sf\n
- dplyr\n
- tigris\n
- readxl\n
- plotly\n
- highcharter\n\n
To install them, run:\n
```R\n
install.packages(c(\"shiny\", \"ggplot2\", \"DT\", \"leaflet\", \"sf\", \"dplyr\", \"tigris\", \"readxl\", \"plotly\", \"highcharter\"))\n
```\n\n
## License\n\n
This project is open source and available under the MIT License. Feel free to use, modify, and distribute as needed.\n\n
## Acknowledgements\n\n
This application uses data from the Federal Elections Commission and geographic data from the U.S. Census Bureau. Special thanks to the developers of the R packages used in this application for providing tools to make data visualization easier.\n\n
## Contact\n\n
For questions or suggestions, please contact the developer at name@university.edu.\n
