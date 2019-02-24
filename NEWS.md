# projmgr 0.0.0.9901

# projmgr 0.0.0.9900

* Added `report_taskboard()` as HTML / CSS alternative to `viz_taskboard()`
* Refactored `viz` functions to reduce dependencies:
    - `ggplot2` moved to Suggests and checked for globally
    - reverted to standard evaluation since NSE was only used is two places to enable non-quoted variable names
* Removed former support in `viz_waterfall()` to respect a `tibble`'s `group_vars` due to questionable value and inconsistency with other functions    
    
# projmgr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.



