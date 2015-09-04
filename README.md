# isotopia

In stable isotope geochemical calculations, we use a number of different representations of isotopic information and processes (ratios, abundances, delta values, alpha values, epsilon values, fractionation factors, refereence frame shifts, mass balance calculations, mass-independent effects, etc., etc.) that are constantly being converted back and forth and used for different kinds of isotope arithmetic. Very frequently, the tangle of keeping track of this information and how all the calculations are done properly makes code very hard to read, difficult to communicate - or even understand oneself later on, and as anyone knows who's ever dropped a -1 or x1000 at the wrong place, prone to small mistakes that can make a huge difference.

The **isotopia** package uses the S4 object system of R to define elemental isotopic data classes so that it can automatically keep track of what is a ratio, what is a delta value (and is it in permil notation or in ppm), etc., and perform isotope arithmetic accordingly. The multiple dispatch system of S4 allows any generic function to be dispached to a method based on the class of the argument, i.e. a fractionation function can be implemented differentely whether it is supposed to fractionate an isotope ratio or a delta value. This allows the user to focus on the actual calculations and communicate to the reader exactly what each value represents. Most importantly, the isotope value object structure allows **isotopia** to put safeguards in place against non-sense calculations and makes it easy to implement rigorous, automatically executed tests (Wickham, 2011) for every single formula and computation (currently there are over 350 tests implemented, see [Testing](#testing) for a few examples). This means that any time any of the isotopia source code is modified, it has to pass all the tests that ensure it is functioning exactly as expected. This kind of test-driven implementation provides high confidence in the calculations and protects from small code changes leading to incorrect results and interpretation. 

The **isotopia** module thus provides several isotopic data types that can be initialized by calling the respective ```ratio```, ```abundance```, ```delta```, ```fractionation_factor``` and ```intensity``` functions. Each data type has additional attributes (such as name of the minor and major isotopes, and what compound it represents, what the reference ratio is for delta values, and what notation is used), which are all described in great detail in the help functions of **isotopia** that are accessible directly from R. Each data type can be initialized as a single vector of isotopic data or an entire system of isotope values for the same element (e.g. all oxygen or all sulfur isotopes). As all isotope data objects are implemented as extensions of primitive data types in R, they can be structured and aggregated in all the ways familiar to people with prior R experience, but should be intuitive enough to be useful "out of the box" for users new to this language. Isotope data types can then be converted from one type to another using ```to_ratio```, ```to_abundance```, ```to_delta```, ```to_fractionation_factor``` methods, can be used in operations (```mass_balance```, ```fractionation```, etc.) or transferred from one notation to another using switch_notation. Here, I provide a few examples how **isotopia** works and how it can be used, with the complete documentation available in the reference manual.

## Installation

The **isotopia** R module can be installed directly from [GitHub](https://github.com/), by using the R development tools module [#devtools2014]. The version of **isotopia** that is used throughout this tutorial is v0.4. I recommend installing this version for the purpose of running any of these code examples locally because **isotopia** is still under active development and the development version includes additional functionality with syntax that might not be backwards compatible. This tutorial will be updated as new versions are released.


```r
install.packages('devtools', depen=T) 
library(devtools)
install_github('isotopia', 'sebkopf', ref = "v0.4")
```

## Data types

After *isotopia* is installed, it can be loaded at any time like any other R module using ```library(isotopia)```. The basic data types are initialized simply by calling the respective ```ratio```, ```abundance```, ```delta``` and ```fractionation_factor``` functions with single or multiple values.


```r
library(isotopia)
show(ratio(0.1)) 
```

```
## An isotope value object of type 'Ratio value': R
## [1] 0.1
```

```r
show(abundance(c(0.1, 0.2))) 
```

```
## An isotope value object of type 'Abundance value': F
## [1] 0.1 0.2
```

```r
show(delta(100, notation = "permil")) 
```

```
## An isotope value object of type 'Delta value': d [permil]
## [1] 100
```

```r
show(fractionation_factor(seq(0.97, 1.03, by=0.01), notation = "alpha"))
```

```
## An isotope value object of type 'FractionationFactor value': alpha
## [1] 0.97 0.98 0.99 1.00 1.01 1.02 1.03
```

```r
show(intensity(100, unit = "mV"))
```

```
## An isotope value object of type 'Intensity value':  [mV]
## [1] 100
```

#### Attributes

All data types have certain attributes that are stored with the data values. For example, an isotope ratio can specify what minor and major isotope it represents and what compound it belongs to.


```r
show(ratio(`13C` = 0.011, major = "12C", compound = "CO2"))
```

```
## An isotope value object of type 'Ratio value': CO2 R 13C/12C
## [1] 0.011
```

And a fractionation factor, for example, can additionally describe what the two reservoirs are between which it fractionates (introducing the shortcut ff instead of the identical long version fractionation_factor in the following).


```r
show(ff(`13C` = 0.995,  major = "12C", ctop = "CO2", cbot = "DIC"))
```

```
## An isotope value object of type 'FractionationFactor value': 13C alpha_CO2/DIC
## [1] 0.995
```

All attributes can also be changed on an already initialized object using the set_attrib() function. However, changing previously defined attributes will always trigger a warning to alert the user to the fact that they are overwriting an attribute.


```r
r <- ratio(`18O` = 0.002, major = "16O", compound = "CO2") 
r <- set_attrib(r, minor = "17O")
```

```
## Warning: changing the isotope name ('Ratio value' object) from '18O' to
## '17O'
```

```r
show(r)
```

```
## An isotope value object of type 'Ratio value': CO2 R 17O/16O
## [1] 0.002
```

There are also a large number of safeguards in place that trigger errors if non-sensical isotope values are initialized (for example a negative isotope ratio or alpha fractionation factor).

#### Isotope systems

Entire isotope systems can be initialized in identical ways, by simply passing several separate values (or entire sequences of values) to the initialization functions (introducing the shortcut ab instead of the identical long version abundance in the following).


```r
show(ab(`33S` = 0.0075, `34S` = 0.0421, `36S` = 0.0002, major = "32S"))
```

```
## An isotope system object of type 'Abundances' with F 33S, F 34S, F 36S
##      33S    34S   36S
## 1 0.0075 0.0421 2e-04
```

## Notation

Closely related to the attributes system is the notation system implemented in **isotopia**. Notation is special because it is an attribute that, when changed, also changes the numerical value of an isotope object with it. All isotope value objects keep track internally what notation they are in, which allows them to be used correctly in any operations and conversions completely independent of what notation the user prefers to work in. Notation is first specified when an isotope value object is initialized and several different notations are implemented for the different isotope value objects. If not specified during intialization, **isotopia** assumes tha an object is created with its default notation. A number of default settings can be specified and retrieved using ```set_iso_opts()``` and ```get_iso_opts()```. Here an example of checking and setting the default notation for fractionation factors (which can be either $\alpha$ values, raw $\epsilon=\alpha-1$ or $\epsilon$ values in permil notation), initializing a new object with default notation (i.e. without specifying notation="x"" during initialization) and converting it back and forth. 


```r
show(get_iso_opts("default_ff_notation")) 
```

```
## [1] "alpha"
```

```r
show(ff(1.02)) # alpha notation 
```

```
## An isotope value object of type 'FractionationFactor value': alpha
## [1] 1.02
```

```r
set_iso_opts(default_ff_notation = "permil") 
show(p <- ff(20))  # permil notation 
```

```
## An isotope value object of type 'FractionationFactor value': eps [permil]
## [1] 20
```

```r
show(switch_notation(p, "eps")) 
```

```
## An isotope value object of type 'FractionationFactor value': eps
## [1] 0.02
```

```r
show(switch_notation(p, "alpha"))
```

```
## An isotope value object of type 'FractionationFactor value': alpha
## [1] 1.02
```

It is important to note that of course all of these values are equivalent, they are just representions of the same fractionation factor in different notation. Accordingly, they behave **exactly** the same in all calculations implemented by **isotopia** regardless what notation they are in.

## Conversions


```r
i <- intensity(`32S` = 9502, `33S` = 75, `34S` = 421, `36S` = 2, 
               major = "32S", unit = "#")
show(i)
```

```
## An isotope system object of type 'Intensities' with 32S [#], 33S [#], 34S [#], 36S [#]
##    32S 33S 34S 36S
## 1 9502  75 421   2
```

```r
r <- to_ratio(i)
show(r)
```

```
## An isotope system object of type 'Ratios' with R 33S/32S, R 34S/32S, R 36S/32S
##        33S     34S       36S
## 1 0.007893 0.04431 0.0002105
```

```r
ab <- to_abundance(r)
show(ab)
```

```
## An isotope system object of type 'Abundances' with F 33S, F 34S, F 36S
##      33S    34S   36S
## 1 0.0075 0.0421 2e-04
```

Because the system of intensities (here as ion counts #) had the major isotope attribute specified, the conversion ```to_ratio``` could automatically figure out what ratios to form. Without specifying which one is the major isotope, the intensities would have still initialized just fine but **isotopia** would have thrown an error when trying to convert to isotope ratios. There's much more functionality in the conversions, which are all listed in the reference manual available directly within R by calling ```?isotopia``` or ```?to_ratio``` or any other function defined in the module.

### Delta values and reference standards

In the case of delta values, conversions often require the specification or use of a reference ratio. This can simply be done by specifying the reference ratio when converting to_delta and since **isotopia** stores the reference ratio with the delta value object, it can be used automatically in the reverse calculation.


```r
r <- ratio(`13C` = 0.0115, major = "12C")
ref_r <- ratio(`13C` = 0.011237, major = "12C", compound = "VPDB")
d <- to_delta(r, ref_ratio = ref_r)
show(d)
```

```
## An isotope value object of type 'Delta value': d13C [permil] vs. VPDB
## [1] 23.4
```

```r
show(to_ratio(d))
```

```
## An isotope value object of type 'Ratio value': R 13C/12C
## [1] 0.0115
```

Additionally, **isotopia** keeps a register of known reference materials with several default values already entered and the possibility for the user to add additional ones they want to use (with the ```register_standard()``` function). Standards can be retrieved as ratio objects by calling ```get_standard()``` and specifying which standard to retrieve for which isotope (see the manual in section A.2.8 for details). The list of all registered ratios can be retrieved as any other option with a call to ```get_iso_opts``` (here turned into a table with the k-table or ```kable``` command provided by the knitr module - Xie, 2013):


```r
library(knitr)
kable(get_iso_opts("standards"), format="markdown")
```



|minor |major |name  |  ratio|
|:-----|:-----|:-----|------:|
|2H    |1H    |VSMOW | 0.0002|
|13C   |12C   |VPDB  | 0.0112|
|15N   |14N   |Air   | 0.0037|
|18O   |16O   |VSMOW | 0.0020|
|34S   |32S   |CDT   | 0.0045|

Registered standards provide **isotopia** with the means to automatically select the correct reference ratio during conversions with delta objects that have sufficiently specific attributes (a message informs the user what was selected, if not enough information is provided to match exactly to one correct standard, this will fail with an error unless the user specifically provides a reference ratio for the conversion).



```r
d <- delta(`2H` = 100, major = "1H", ref = "VSMOW")
show(d)
```

```
## An isotope value object of type 'Delta value': d2H [permil] vs. VSMOW
## [1] 100
```

```r
show(to_ratio(d))
```

```
## An isotope value object of type 'Ratio value': R 2H/1H
## [1] 0.0001713
```

## Operations

With the conversions and data types all in place, **isotopia** can easily expand its func- tionality by building on top of the data types. Currently, operations are limited to a number of key features, such as ```mass_balance()``` calculations for fractional abundances and delta values, as well as fractionating (```fractionate()```) isotope data objects with ```fractionation_factors``` and shifting the reference frame on delta values (```shift_reference()```). As always, attributes are carried through these operations in the most sensible way for what they actuallly represent.

### Mass balance

Mass balance makes use of an additional attribute not mentioned before, the weight attribute. This allows one to weight values according to their reservoir sizes such that during mass balance calculations, isotopically different pools are mixed according to their relative proportions. Weight can be specified either during initialization or by using the ```weight()``` function later on. Imagine a reservoir of CO2 that receives a tiny spike of heavily labeled additional carbon. For convenience, we're introducing here the **isotopia** options to set the default minor and major isotope names - this is nice for working on a problem in a specific isotope system. Also, we're going to do the mass balance exact by converting to fractional abundances (although **isotopia** provides the approximate ```mass_balance()``` directly with delta value objects as well).


```r
set_iso_opts(
    default_minor = "13C", 
    default_major="12C",
    default_ab_notation = "percent")
res <- delta(-10, compound = "CO2", ref = "VPDB", weight = 100)
show(res)
```

```
## A weighted isotope value object of type 'Delta value': CO2 d13C [permil] vs. VPDB
##   value weight
## 1   -10    100
```

```r
spike <- ab(seq(5, 25, by = 5), compound = "Cspike")
show(spike)
```

```
## An isotope value object of type 'Abundance value': Cspike F 13C [%]
## [1]  5 10 15 20 25
```

```r
mb <- mass_balance(
    to_ab(res), # convert reservoir to abundance
    weight(spike, 0.1) #weight spike
)
show(mb)
```

```
## A weighted isotope value object of type 'Abundance value': CO2+Cspike F 13C [%]
##   value weight
## 1 1.104  100.1
## 2 1.109  100.1
## 3 1.114  100.1
## 4 1.119  100.1
## 5 1.124  100.1
```

Notice that the result of the mass balance again is a weighted isotope value object itself. It can be converted to other data types or you can keep adding additional components to it with mass balance calculations. In fact, since **isotopia** keeps track of the weight, you can keep tagging multiple mass balances together (the ```mass_balance()``` function takes as many parameters as desired). Additionally, since R implements basic arithmetic operators as functions, **isotopia** redefines adding (+) and subtracting (-) for abundance and delta objects to be interpreted as mass balance calculations. This means ```mass_balance(x, y)``` is the same as ```x + y``` for these isotope value objects. This allows short-hand calculations like the following (although ```mass_balance()``` is recommended in more complex situations for clarity of recording what is happening). Here, we are adding a heavy relatively small but heavy pool (40permil, weight=2) to a circumneutral reservoir (5permil, weight=10) and then remove an isotopically light fraction from the pool (-10permil, weight=4).


```r
mb <- 
    delta(5, weight = 10) + 
    delta(40, weight = 2) - 
    delta(-10, weight = 4)
show(mb)
```

```
## A weighted isotope value object of type 'Delta value': d13C [permil]
##   value weight
## 1 21.25      8
```

### Fractionate

During fractionation, a fractionation factor modifies an isotope value object (for example an isotope ratio or a delta value).


```r
a <- ff(1.05, ctop = "DIC", cbot = "CO2", notation = "alpha")
r <- ratio(0.114, compound = "CO2")
r <- fractionate(a, r)
show(r)
```

```
## An isotope value object of type 'Ratio value': DIC R 13C/12C
## [1] 0.1197
```

Notice that **isotopia** automatically keeps track of what compound is represented. Af- ter fractionation, the ratio represents no longer $CO[2]$ but $DIC$ according to the fractionation factors attributes. If these attributes do not "cancel" correctly, this command fails with an error and the relevant error message. Same as with ```mass_balance()```, **isotopia** implements arithmetic shorthand for this, isotope value objects can be simply fractionationed by multiplying with a fractionation factor. I.e., ```fractionate(a, b)``` is the same as ```a * b``` (this also means fractionation factors can be easily chained with ```a1 * a2 * a3 * b``` but only if the "numerators" and "denominators" cancel properly).


```r
ff(-25, notation = "permil", ctop = "Corg", cbot = "DIC") *
    ff(-5, notation = "permil", ctop = "DIC", cbot = "CO2") * 
        delta(100, compound = "CO2")
```

```
## An isotope value object of type 'Delta value': Corg d13C [permil]
## [1] 67.14
```

### Shift reference

The last operation to introduce for now is shifting a reference frame. This is only defined for ```delta``` values and requires the denominator and numerator to cancel (otherwise fails with an error). It is also implemente with the ```delta * delta``` arithmetic shorthand. This is a typical scenario useful for processing laboratory data which is measured against a standard of known isotopic composition relative to an international reference.



```r
sample <- delta(-5, compound = "sample", ref = "my_std")
standard <- delta(-2.5, compound = "my_std", ref = "VPDB")
show(shift_reference(sample, standard))
```

```
## An isotope value object of type 'Delta value': sample d13C [permil] vs. VPDB
## [1] -7.487
```

```r
show(sample * standard)
```

```
## An isotope value object of type 'Delta value': sample d13C [permil] vs. VPDB
## [1] -7.487
```


### Arithmetic

Several of the arithmetic shorthands were introduced already, but there are several more (for all, see the manual). For all of these, it is always recommend to use the actual real functions in more complex scenarios for clarity. Here's just an example of what **isotopia** can automatically keep track of in terms of isotope data objects. Here are two ways of turning isotope ratios into a fractionation factor in permil notation - it works booth by explicit mention of each functional step, or by the arithmetic equivalent.


```r
r1 <- ratio(0.011)
r2 <- ratio(0.0113)
p <- switch_notation(to_ff(r1, r2), "permil")
show(p)
```

```
## An isotope value object of type 'FractionationFactor value': 13C eps [permil]
## [1] -26.55
```

```r
p <- (r1/r2 - 1) * 1000
show(p)
```

```
## An isotope value object of type 'FractionationFactor value': 13C eps [permil]
## [1] -26.55
```

## Testing

Testing of all functionality in **isotopia** is implemented using the **testthat** module (Wickham, 2011), which provides a simple and uniform way of writing tests that can be run automatically to report any incorrect behaviour immediately. This enables multiple developers to contribute to the core functionality of the project without the risk of breaking prior implementations, but also allows users to easily write a few tests of their own to be confident that the module is doing what it is supposed to be doing, or just to test their own code and formulas on a regular basis. Here are few examples from the many tests already implemented for **isotopia** to give an idea of the range of functionality tests:


```r
library(testthat)
set_iso_opts(default_ab_notation = "raw", 
             default_delta_notation = "permil", 
             default_ff_notation = "alpha")
expect_error(ratio(-0.2), "cannot be negative")
expect_false(is.ratio(abundance(0.1)))
expect_equal(to_ff(delta(200), delta(-200)), ff(1.2 / 0.8))
expect_is({
        amix <- abundance(`13C` = 0.2, weight = 2, compound = "a") + 
            abundance(`13C` = 0.5, compound = "b") + 
            abundance(`13C` = 0.3, weight = 3, compound = "c")}, "Abundance")

expect_equal(get_label(amix), "a+b+c F 13C") 
expect_equal(get_value(amix), (0.2*2 + 0.5 + 0.3*3) / (2+1+3))
expect_equal(get_value(ff(0.8) * delta(200) + delta(100), "permil"), 1000*(0.8*1.2 - 1 + 0.1)/2)
```

And this is what happens as soon as a test fails (here have to catch the error, otherwise this document would not compile):


```r
tryCatch(
expect_equal(fractionate(ff(0.995), delta(42)), delta(42)),
error = function(e) show(e))
```

```
## <simpleError: fractionate(ff(0.995), delta(42)) not equal to delta(42)
## Mean relative difference: 0.124>
```

## Future extensions

The **isotopia** package currently implements a lot of the core functionality for isotope arithmetic. However, there is much that could built on top of it, including support for mass-scaling and mass-independent data objects and multiply subsituted isotopologues. The goal with all of these would be to provide an interface that can implement rigorous unit tests to ensure calculations are always performed the exact same way, tools to convert between reference frames and make it easier to compare and visualize data in different isotopic spaces, and, above all, to make it fun, intuitive and reproducible to work with isotopic data.

## References
 
  - [Wickham, H., 2011. testthat: Get started with testing. The R Journal 3, 5-10.](http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf)
  - [Xie, Y., 2013b. knitr: A general-purpose package for dynamic report generation in R.](http://cran.r-project.org/web/packages/knitr/index.html)
