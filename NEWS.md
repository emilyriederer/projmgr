# projmgr (development version)

# projmgr 0.1.0

* Enhanced test suite to prepare for official release
* First version sent to CRAN

# projmgr 0.0.0.9902

* Tweaks to `report` family of functions to exposure more options to end users

# projmgr 0.0.0.9901

* Added `listcol_extract()`, `listcol_pivot()` and `listcol_filter()` functions to work with assignee and label list-columns
* Added new arguments to `report_progress()` 
    - `group_var` allows issues to do be grouped by variables than `milestone_title` (which remains the default)
    - `show_stats` allows for suppression of count/percent output in group titles
* Improved documentation for using `create_repo_ref()` without authentication

# projmgr 0.0.0.9900

* Added `report_taskboard()` as HTML / CSS alternative to `viz_taskboard()`
* Refactored `viz` functions to reduce dependencies:
    - `ggplot2` moved to Suggests and checked for globally
    - reverted to standard evaluation since NSE was only used is two places to enable non-quoted variable names
* Removed former support in `viz_waterfall()` to respect a `tibble`'s `group_vars` due to questionable value and inconsistency with other functions    
    
# projmgr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.



