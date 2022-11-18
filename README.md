# AvE - Actual vs Expected
### There are many machine learning visualisation tools out there for assessing goodness of fit. This is the one that is interactive and intuitive!
*A POC*

Features:
1. View actual and predicted values in one graph
2. Select amongst all predictor variables with a drop-down button - dynamically graphs the selected variable on the x-axis
3. Shows predicted median and percentiles in addition to the standard "single-point-estimate" mean value that most machine learning models output. Percentiles are determined by variying the selected predictor (step #2) and holding all other predictor variables constant.
4. Dynamically updates field filters that can be applied - filters may be applied only to fields that are not graphed (the selected x-axis and the response variable). Changes filter type (slider or drop down) depending on whether the field is numeric or discrete
5. Capable of graphing *any* model - GLM, Decision trees, neural network, home-made...

![alt text](https://github.com/jack0reardon/AvE/tree/main/inst/www/screenshot_1.png?raw=TRUE "The User Interface")
