# Generalised Linear Modelling and Model Interpretation
## When linear model assumptions are violated

##### Created by Kristina McGuinness

### Tutorial Aims

1. [Check linear model assumptions](#1-check-linear-model-assumptions)
2. [What is a GLM?](#2-what-is-a-glm)
    - [Build the GLM](#build-the-glm)
    - [Interpret the GLM](#interpret-the-glm)
3. [Visualise population trends](#3-visualise-population-trends)
    - [Add predicted trends based on the model](#add-predicted-trends-based-on-the-model)


---------------------------

### Introduction
Models are an important part of science, allowing us to test for and show correlations between different variables – for example, if an increase in rainfall correlates to an increase in plant height. If you are familiar with linear models, you are likely familiar with their 3 assumptions: all observations are independent, the residuals are normally distributed, and the residuals show equal variance. However, what happens when one of these assumptions are violated? In this tutorial we will use generalised linear models (glms) to deal with this exact situation, and also discuss model interpretation.

For the model, we will use population data on arctic foxes in Sweden, taken from the WWF's Living Planet Index. The full LPI can be found [here](https://www.livingplanetindex.org/search). We will also use `ggplot2` package to visualise trends. For those not familiar with `ggplot2`, or those who would like a refresher, check out one of these tutorials on data visualisation:
    - [Beautiful and Informative Data Visualisation](https://ourcodingclub.github.io/tutorials/datavis/)
    - [Customising your figures](https://ourcodingclub.github.io/tutorials/data-vis-2/)

This tutorial also assumes knowledge of linear models, so be sure to check out [this tutorial](https://ourcodingclub.github.io/tutorials/modelling/) if you are unfamiliar with them, or need a refresher.

All the files you need to complete this tutorial can be downloaded from [this repository](https://github.com/EdDataScienceEES/tutorial-kristinalemon).
Click on Code/Download ZIP and unzip the folder, or clone the repository to your own GitHub account.


## 1. Check linear model assumptions

Start by opening RStudio, then set your working directory and load the relevant packages and LPI data.


```r
# Set the working directory
setwd("your_filepath")

# Load packages
library(tidyverse)    # tidyverse includes dplyr and ggplot2, both of which we will need for this tutorial.
library(ggeffects)

# Load LPI data
load("data/LPI_data.Rdata")
```

We can now filter the data and construct a linear model of arctic fox populations in Sweden. Filtering the data is easily done with pipes.
We would like to see if fox populations have increased or decreased over time, so "Population" is our response variable. 
We will create a new column "YearScaled", so the year 1974 will become year 0, 1975 will be year 1, etc. "YearScaled" will then be our independent variable.

```r
# Filter LPI data to only include arctic fox populations in Sweden.
arctic_foxes <- data %>%
  pivot_longer(cols=25:69, names_to = "Year", values_to = "Population") %>%   # converts data to long form, adds columns called "Year" and "Population"
  mutate(Year = parse_number(Year)) %>%                                       # removes non-numeric characters from "Year", i.e. removes the letter X that was in front of each year
  filter(Genus == "Vulpes",
         Species == "lagopus",
         Country.list == "Sweden") %>%                                        # filters for arctic fox populations in Sweden only
  select(-c(X1, Data.source.citation, Sub.species, Authority)) %>%            # removes unnecessary columns
  subset(!is.na(Population)) %>%                                              # removes rows where Population = NA
  subset(Population != 0.00)                                                  # removes rows where Population = 0 i.e. there were no foxes found

# Explore the data
head(arctic_foxes)
plot(arctic_foxes$Year, arctic_foxes$Population)

# Add YearScaled column so 1974 = year 0, 1975 = year 1 etc.
arctic_foxes <- arctic_foxes %>%
  mutate(YearScaled = (1974 - Year) * -1)

# Convert Population to integer so it is easier to model
arctic_foxes$Population <- as.integer(arctic_foxes$Population)

# Construct a linear model for fox populations
fox_lm <- lm(Population ~ YearScaled, data = arctic_foxes)

```

Now, use `plot()` to plot your model and check if any assumptions have been violated. You will notice that the points on the Q-Q residuals plot do not quite follow a straight line
(in an ideal world, the points would line up with the dotted diagonal line). This means that residuals in our data are likely not normally distributed, violating one of the assumptions of linear modelling!

The residuals vs fitted and scale-location plots are not looking great either. The red line on the residuals vs fitted plot forms a parabola, implying that our data does not follow a linear pattern.
Similarly, the red line on the scale-location plot also forms a parabola, implying that the residuals in our data do not show equal variance, violating another assumption of linear modelling.

So, what can we do to make our model fit the data better? This is where generalised linear models come in!

## 2. What is a GLM?

Generalised linear models (GLMs) are similar to linear models in that they also show correlations between variables, however they are better equipped to handle data where the residuals
are not normally distributed. Residuals not being normally distributed tends to happen with count data, which is what we are dealing with (as we are looking at arctic fox populations, 
more specifically we are looking at the abundance of adults over time).

GLMs use what is called a 'link function', which tells the model what kind of distribution the response variable has. In this case, our data (count data) has a "poisson" distribution.

### Build the GLM

Building a GLM is easy - we just use the `glm()` function instead of `lm()`, then specify the distribution (written as family in the model).

```r
# Build the GLM
fox_glm <- glm(Population ~ YearScaled, data = arctic_foxes, family = "poisson")
plot(fox_glm)

```
Comparing the plots of the two models, the residuals vs fitted plot looks much better for the GLM than the LM. The scale-location plot for the GLM also looks flatter, i.e. the residuals now show more equal variance compared to the LM. The points on the Q-Q residuals plot follow the dotted diagonal line more closely as well, showing that they are now closer to a normal distribution.

***Note***: If you look at the residuals vs leverage plot, you'll notice that the GLM makes it evident that there are some leverage points in the data. We could remove them, however we will leave them in for this tutorial because our dataset is small, so removing them could significantly affect our analysis and interpretation of the model.

### Interpret the GLM

Now, use the `summary()` function on fox_glm. A table will appear in the console that looks like this:

<center><img src="{{ site.baseurl }}/glm_summary_table.png" alt="Img"></center>

But what does this mean? Let's start with the Estimate column. The Estimate for (Intercept) simply refers to where the line of best fit crosses the y-axis if you were to plot the data.
In this case, the line of best fit crosses the y-axis at 4.267. This means that in year 0 (remember, year 0 = 1974 because we are using YearScaled!), the Population is 4.267 foxes.

***Keep in mind that this is the log(Population), and not the actual population at year 0. To find the real population at year 0, we can exponentiate 4.267 with `exp()`. We can then check our answer with `ggpredict()` from the `ggeffects` package.***

```r
# Check summary table for model interpretation
summary(fox_glm)

# From the table, we see that log(Population) in year 0 = 4.267, so use exp() to find the actual Population
exp(4.267)

# There are 71 adult arctic foxes present in year 0. We can check this with ggpredict(). ggpredict() tells us the predicted population values based on our fox_glm model.
ggpredict(fox_glm, terms = c("YearScaled"))

```
The YearScaled Estimate tells us the gradient of the best fit line, i.e. how the population is changing from year to year. Here, the gradient is -0.079.
Once again, we can exponentiate this number to find the percentage change in fox population every year.

```r
# Find the percentage change in population per year by using exp() on the YearScaled Estimate
exp(-0.079)

## From this we get 0.924. This is less than 1, meaning that fox populations are decreasing each year. If we had gotten a number greater than 1, that would indicate that populations are increasing each year instead.
## The difference between 1 and 0.924 will tell us by how much fox populations are decreasing.
1 - 0.924 = 0.076
```
0.076 as a percentage is 7.6%, meaning that each year, fox populations are decreasing by 7.6%.

The other important column in the summary table is the Standard Error column, as this gives us an indication of how confident we can be with our model results.
Generally, the smaller the standard error, the more precise our model is. In our model, we have a small standard error of 0.005 for our YearScaled values, indicating that our model is quite precise in predicting fox population trends.

We can also check the precision of our model using confidence intervals (which is a similar concept to the standard error). The `ggpredict()` function will also tell us these confidence intervals, as you may have noticed when we were checking the number of foxes present in year 0.
Looking at the confidence intervals with `ggpredict()`, we can confirm the precision of our model.

## 3. Visualise population trends

We can now make a figure to go with our model that visualises arctic fox population trends. `ggplot2` is very useful for this, as you can easily make pretty and professional-looking graphs with it.
For example, we can use `geom_point()` to easily make a scatterplot, which is a good plot for visualising population trends. 

```r
# Visualise arctic fox population trends
# Fun fact: if you put brackets () around your entire ggplot code (as I've done below), RStudio will automatically plot your graph without you needing to use plot()!
(fox_plot <- ggplot() +
  geom_point(data = arctic_foxes, aes(x = YearScaled,
                                      y = log(Population)),
                                      colour = "dodgerblue4") +
  labs(x = "Year of study",
       y = "Scaled population",
       title = "Changes in Arctic Fox Populations in Sweden over time") +
  theme_test())
  
```
You should have a graph that looks like this:

![alt text](https://github.com/EdDataScienceEES/tutorial-kristinalemon/blob/master/images-for-tutorial/basic-scatterplot.jpeg)

From this final output, we can see that fox populations are indeed decreasing over time, and the y-intercept value of 4.267 looks pretty accurate too.
However, this graph is looking a bit basic. Let's add predicted population values based on our model.

### Add predicted trends based on the model

We can add predicted population values based on our model by first making a new dataframe of these predicted values. This is done using `data.frame()` and `ggpredict()`. Then, we use `geom_line()` to add a line of best fit based on the dataframe we have just created, followed by using `geom_ribbon()` to add our confidence intervals.

It is good practice to visualise your confidence intervals when making figures to accompany your linear or generalised linear models, as you can easily communicate how precise your model is without needing to tell people your exact standard error or confidence interval values.

```r
# Add predicted values based on the model ----
# Make a dataframe based on model
predicted_foxes <- data.frame(ggpredict(fox_glm, terms = c("YearScaled"))) %>%
  rename(YearScaled = x, Population = predicted)  # rename columns so they are consistent with what we have been calling them in this tutorial


# Plot figure with predicted population values and confidence intervals
## geom_line() adds a line of best fit based on the model
## geom_ribbon() adds confidence intervals around the line of best fit based on the model
(fox_plot <- ggplot() +
    geom_point(data = arctic_foxes, aes(x = YearScaled,
                                        y = log(Population)),
                                        colour = "dodgerblue4") +
    geom_line(data = predicted_foxes, aes(x = YearScaled,
                                          y = log(Population))) +
    geom_ribbon(data = predicted_foxes, aes(x = YearScaled,
                                            ymin = log(conf.low),
                                            ymax = log(conf.high)),
                alpha = 0.4, fill = "deepskyblue") +
    labs(x = "Year of study",
         y = "Scaled population",
         title = "Changes in Arctic Fox Populations in Sweden over time") +
    theme_test())
    
```
Your graph should now look like this: 

![alt text](https://github.com/EdDataScienceEES/tutorial-kristinalemon/blob/master/images-for-tutorial/scatterplot-with-trends.jpeg)

## Summary

That brings us to the end of this tutorial, well done for getting through it! To summarise, we have learned:

##### - what generalised linear models (GLMs) are and when to use them
##### - how to interpret a GLM using the summary table
##### - how to visualise population trends with a scatterplot using `ggplot2`
##### - how to use `ggpredict()`, `geom_line()` and `geom_ribbon()` to add predicted trends to a scatterplot


<hr>
<hr>
For more on `ggplot2`, read the official <a href="https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf" target="_blank">ggplot2 cheatsheet</a>.

#### Check out our <a href="https://ourcodingclub.github.io/links/" target="_blank">Useful links</a> page where you can find loads of guides and cheatsheets.

#### If you have any questions about completing this tutorial, please contact us on ourcodingclub@gmail.com

#### <a href="INSERT_SURVEY_LINK" target="_blank">We would love to hear your feedback on the tutorial, whether you did it in the classroom or online!</a>

<ul class="social-icons">
	<li>
		<h3>
			<a href="https://twitter.com/our_codingclub" target="_blank">&nbsp;Follow our coding adventures on Twitter! <i class="fa fa-twitter"></i></a>
		</h3>
	</li>
</ul>

### &nbsp;&nbsp;Subscribe to our mailing list:
<div class="container">
	<div class="block">
        <!-- subscribe form start -->
		<div class="form-group">
			<form action="https://getsimpleform.com/messages?form_api_token=de1ba2f2f947822946fb6e835437ec78" method="post">
			<div class="form-group">
				<input type='text' class="form-control" name='Email' placeholder="Email" required/>
			</div>
			<div>
                        	<button class="btn btn-default" type='submit'>Subscribe</button>
                    	</div>
                	</form>
		</div>
	</div>
</div>
