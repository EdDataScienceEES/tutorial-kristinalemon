<center><img src="{{ site.baseurl }}/tutheaderbl.png" alt="Img"></center>

To add images, replace `tutheaderbl1.png` with the file name of any image you upload to your GitHub repository.

### Tutorial Aims

1. [Check linear model assumptions](#part1)
2. [What is a GLM?](#part2a)
    - [Build the GLM](#part2b)
    - [Interpret the GLM](#part2c)
3. [Visualise population trends](#part3a)
    - [Add predicted trends based on your model](#part3b)


---------------------------

### Introduction
Models are an important part of science, allowing us to test for and show correlations between different variables – for example, if an increase in rainfall correlates to an increase in plant height.
If you are familiar with linear models, you are likely familiar with their 3 assumptions: all observations are independent, the residuals are normally distributed, and the residuals show equal variance.
However, what happens when one of these assumptions are broken? In this tutorial we will use generalised linear models (glms) to deal with this exact situation, and also discuss model interpretation.

For the model, we will use population data on arctic foxes in Sweden, taken from the WWF's Living Planet Index. The full LPI can be found [here](https://www.livingplanetindex.org/search). We will also use `ggplot2` package to visualise trends.
For those not familiar with `ggplot2`, or those who would like a refresher, check out one of these tutorials on data visualisation:
[Beautiful and Informative Data Visualisation](https://ourcodingclub.github.io/tutorials/datavis/)
[Customising your figures](https://ourcodingclub.github.io/tutorials/data-vis-2/)

This tutorial also assumes knowledge of linear models, so be sure to check out [this tutorial](https://ourcodingclub.github.io/tutorials/modelling/) if you are unfamiliar with them, or need a refresher.

{% capture callout %} All the files you need to complete this tutorial can be downloaded from [this repository](https://github.com/EdDataScienceEES/tutorial-kristinalemon).
Click on Code/Download ZIP and unzip the folder, or clone the repository to your own GitHub account. {% endcapture %} {% include callout.html content=callout colour=alert %}


## 1. Check linear model assumptions
{: #part1}

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
  subset(Population != 0.00)

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
(the points should line up with the dotted diagonal line). This means that residuals in our data are likely not normally distributed, violating one of the assumptions of linear modelling!

The residuals vs fitted and scale-location plots are not looking great either. The red line on the residuals vs fitted plot forms a parabola, implying that our data does not follow a linear pattern.
Similarly, the red line on the scale-location plot also forms a parabola, implying that the residuals in our data do not show equal variance, violating another assumption of linear modelling.

So, what can we do to make our model fit the data better? This is where generalised linear models come in!

## 2. What is a GLM?
{: #part2a}

Generalised linear models (GLMs) are similar to linear models in that they also show correlations between variables, however they are better equipped to handle data where the residuals
are not normally distributed. Residuals not being normally distributed tends to happen with count data, which is what we are dealing with (as we are looking at arctic fox populations, 
more specifically we are looking at the abundance of adults over time).

GLMs use what is called a 'link function', which tells the model what kind of distribution the reponse variable has. In this case, our data (count data) has a "poisson" distribution.

### Build the GLM
{: #part2b}

Building a GLM is easy - we just use the `glm()` function instead of `lm()`, then specify the distribution (written as family in the model).

```r
# Build the GLM
fox_glm <- glm(Population ~ YearScaled, data = arctic_foxes, family = "poisson")
plot(fox_glm)

```
Comparing the plots of the two models, the residuals vs fitted plot looks much better for the GLM than the LM. The scale-location plot for the GLM also looks flatter, i.e. the residuals now show more
equal variance compared to the LM. The points on the Q-Q residuals plot follow the dotted diagonal line more closely as well, showing that they are now closer to a normal distribution.

**Note**: If you look at the residuals vs leverage plot, you'll notice that the GLM makes it evident that there are some leverage points in the data. We could remove them,
however we will leave them in for this tutorial because our dataset is small, so removing them could significantly affect our analysis and interpretation of the model.

### Interpret the GLM
{: #part2c}



## 3. Visualise population trends
{: #part3a}


### Add predicted trends based on your model
{: #part3b}


This is the end of the tutorial. Summarise what the student has learned, possibly even with a list of learning outcomes. In this tutorial we learned:

##### - how to generate fake bivariate data
##### - how to create a scatterplot in ggplot2
##### - some of the different plot methods in ggplot2

We can also provide some useful links, include a contact form and a way to send feedback.

For more on `ggplot2`, read the official <a href="https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf" target="_blank">ggplot2 cheatsheet</a>.

Everything below this is footer material - text and links that appears at the end of all of your tutorials.

<hr>
<hr>

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
