# Competition - Morgan Stanley's Quant Challange
Morgan Stanley's Quant Challenge Qualifier Competition

## About the competition
Morgan Stanley’s Quant Challenge is a chance to experience first-hand how quantitative analytics teams work within a global investment bank.
Use your skills in data analysis, modelling and model-testing to solve complex problems, inspired by real challenges our Firm’s analysts tackle every day.

## Description of the Challenge
How would you forecast the effects of climate change on agricultural businesses? Help an imaginary bank find an innovative solution to support its clients in the agriculture sector.
In the qualification round, we were required to use our analytical and programming skills to build a model that estimates future crop yields as a function of weather, based on historical data.

In our project as a Risk Analyst team, we supported an American credit institution that focuses on agricultural investments by predicting agricultural crop yields in Minnesota, considering climate change impacts. The analysis involved several key steps:

**Data Understanding and Preparation**: We began by analyzing the provided datasets, which included historical crop yields and weather data. We matched county data with the nearest weather stations and aggregated the weather data to align it with crop data, calculating annual averages for temperatures and total precipitation.

**Data Cleaning**: We addressed missing values and outliers in the datasets to ensure the integrity of our analysis.

**Model Development**: We built multiple linear multivariate OLS regression models with increasing complexity to predict crop yields based on weather variables, the year, and other factors. Our strategy was to start with simpler models and progressively include more variables to monitor for overfitting.

**Model Evaluation**: We split our dataset into training and testing subsets and performed k-fold cross-validation to evaluate the models. The model with the best predictive power was selected based on its Root Mean Square Error (RMSE).

**Final Model Selection**: After thorough evaluation, we chose a transformed linear multivariate OLS regression model for our final predictions, as it demonstrated a strong ability to predict yields based on the available data.

### Qualifier results
121 Teams overall
Congratulations to the teams has been qualified to the Final round:
- Cash Money
- Compacto
- Dagobert Investments
- Ezperez
- GameRank2
- Hundred
- **ProgramRs** (our team)
- RIO

**Disclaimer:** This analysis was conducted as part of a competition organized by Morgan Stanley Hungary. Due to the nature of this competition, we are unable to share any data or specific results from the project, as we have signed a Non-Disclosure Agreement (NDA) regarding the confidentiality of the information used. All methodologies and findings presented are based on our own interpretations and analyses within the scope of the competition.
