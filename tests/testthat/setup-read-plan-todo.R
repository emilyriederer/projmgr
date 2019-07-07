plan <-
"
- title: Data cleaning and validation
  description: >
    We will conduct data quality checks,
    resolve issues with data quality, and
    document this process
  due_on: 2018-12-31T12:59:59Z
  issue:
  - title: Define data quality standards
    body: List out decision rules to check data quality
    assignees: [emilyriederer]
    labels: [a, b, c]
  - title: Assess data quality
    body: Use assertthat to test decision rules on dataset
    labels: [low]
  - title: Resolve data quality issues
    body: Conduct needed research to resolve any issues

- title: Exploratory data analysis
  description: >
    Create basic statistics and views to better
    understand dataset and relationships
  issue:
  - title: Summary statistics
    body: Calculate summary statistics
  - title: Visualizations
    body: Create univariate and bivariate plots
"

todo <-
"
- title: Consider renaming my_very_long_function_name
  body: >
    This name is unneccesarily long and hence
    it is very hard and annoying to type
    assignees: [emilyriederer]
  labels: [low]

- title: Make sure documentation links right pages of API docs
"
