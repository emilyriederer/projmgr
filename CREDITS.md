# Credits

Throughout development of this package, I drew much inspiration from a variety of workflow-enhancing R packages and learned a lot from blog posts and source code. This section gives kudos to those resources and their creators. The one caveat is that this list is almost surely incomplete. 

## Package Design

`ghtrackr`'s emphasis on project management means that it aspires to the class of workflow enhancing packages. Such packages don't take center stage in data analysis but instead attempt to improve rote processes and reduce the mental overhead of switching contexts between one's analysis and other ancillary tasks. 

Two packages that exemplify workflow enhancement are `pkgdown` and `usethis`. They inspired me to think broadly about this type of package and also specifically inspired certain functionality and implementation within `ghtrackr`. In particular, `pkgdown` inspired my use of YAML templates for project plans and the inclusion of `template_` functions to demonstrate how that YAML should be structured. `usethis` includes many `browse_` functions to help users navigate to various websites easily, and I borrowed this concept and implementation for `ghtrackr`'s set of `browse_` functions.

## Using APIs in R

Before you write an API wrapper, you have to learn the basics of APIs. Here are a few great talks on using the `httr` R package:

- Amanda Gardrow's rstudio::conf 2017 talk, [Using Web APIs in R](https://www.rstudio.com/resources/videos/using-web-apis-from-r/) from rstudio::conf 2017
- Lucy D'Agostino McGowan's JSM 2018 talk, [Harnessing the Power of the Web via R Clients for Web APIs](https://www.lucymcgowan.com/talk/asa_joint_statistical_meeting_2018/) 

## Writing API Packages in R

- Hadley Wickham's `httr` vigneete [Best practices for API packages](https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html)
- Colin Fay's blog [How to Build an API Wrapper Package in 10 Minutes](https://colinfay.me/build-api-wrapper-package-r/)

## Example API Packages

I prowled through countless GitHub repos on this journey, but here are a few API packages that I kept digging into particularly to see if what I was doing made sense.

- Kyle Walker's [tidycensus](https://github.com/walkerke/tidycensus) package
- Charlie Thompson's [spotifyr](https://github.com/charlie86/spotifyr) package
- Amanda Dobbyn's [postal](https://github.com/aedobbyn/postal) package
- Mine Cetinkaya-Rundel's [ghclass](https://github.com/mine-cetinkaya-rundel/ghclass) package
- RStudio's [gh](https://github.com/r-lib/gh) package

## R Package Development

Finally, while I was not new to package development, this process was of course facilitated by the phenomanl resources that are:

- Hadley Wickham's [R Packages](http://r-pkgs.had.co.nz/) book
- Hadley Wickham's [Advanced R](http://adv-r.had.co.nz/) book
- Maelle Salmon's [How to Develop a Good R Package (for Open Science)](https://masalmon.eu/2017/12/11/goodrpackages/)
- The [devtools](https://devtools.r-lib.org/) package
- The [usethis](https://usethis.r-lib.org/) package

