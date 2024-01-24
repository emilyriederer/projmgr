# projmgr (development version)

# projmgr 0.1.1

* Fixed documentation error in `parse_issue_events()` causing a new CRAN note

# projmgr 0.1.0.9000

* Fixed error in `has_n_commits()` taskboard helper and unit test to attribute events to the correct issues
* Changed behavior of `get_issue_events()` to create minimal dummy event 
    - Dummy events are identifiable by  `id = -9999` and `event = "exists"`. Other fields not populated to emphasize non-standard event type. 
    - This enables easier mapping. For example, users may now write `purrr::map_df(issues$number, ~get_issue_events(my_repo, .x) %>% parse_issue_events())` which would have previously thrown an error
    - New behavior can be disabled with new `dummy_events` argument

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



