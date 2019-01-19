report_taskboard <- function(data, in_progress_when, include_link = FALSE, hover = FALSE){

  stopifnot(is.function(in_progress_when))

  # create datasets by group ----
  done <- data[data$state == "closed",]
  inprog <- data[in_progress_when(data),]
  todo <- data[data$state != "closed" & !in_progress_when(data), ]

  # define task generator helper fx ----
  generate_task_html <- function(class, link, url, title){

    paste(
      "<div class = 'task", class, "'>",
      ifelse(link, paste0("<a href = '", url, "' target='_blank'>"), ""),
      title,
      ifelse(link, "</a>", ""),
      "</div>"
    )

  }

  # html ----
  done_html <- mapply( function(url, title) generate_task_html("dn", include_link, url, title), done$url, done$title )
  inprog_html <- mapply( function(url, title) generate_task_html("ip", include_link, url, title), inprog$url, inprog$title )
  todo_html <- mapply( function(url, title) generate_task_html("ns", include_link, url, title), todo$url, todo$title )

  # add blank divs to balance grid ----
  n_td <- nrow(todo)
  n_ip <- nrow(inprog)
  n_dn <- nrow(done)
  n_max <- max(n_td, n_ip, n_dn, na.rm = TRUE)
  if(n_td < n_max){todo_html[n_td+1:n_max] <- "<div></div>"}
  if(n_ip < n_max){inprog_html[n_ip+1:n_max] <- "<div></div>"}
  if(n_dn < n_max){done_html[n_dn+1:n_max] <- "<div></div>"}
  html_tasks <- paste(paste(todo_html, inprog_html, done_html), collapse = "")
  html <- paste(
    "<div class = 'taskboard'>
    <div class = 'head'>To Do</div>
    <div class = 'head'>In Progress</div>
    <div class = 'head'>Done</div>",
    html_tasks,
    "</div>")


  # style ----
  css <-
    "
  .taskboard{
  display: grid;
  grid-gap: 1%;
  grid-template-columns: 1fr 1fr 1fr;
  margin-bottom: 50px;
  }

  .head{
  background-color: #a9a9a9;
  border: 1px solid #d3d3d3;
  text-align: center;
  font-weight: strong;
  }

  .task{
  text-align: center;
  padding: 2%;
  margin: 2%;
  border: 1px solid #d3d3d3;
  box-shadow: 2px 2px 5px grey;
  transition-duration: 0.5s;
  }

  .ns{background-color: #f0e442;}
  .ip{background-color: #56b4e9;}
  .dn{background-color: #009e73;}

  a, a:visited {
  color: black;
  text-decoration: none;
  }

  a:hover{
  color: black;
  text-decoration: underline;}

  a:active{color: white;}
  "

  css <- paste(
      "<style>",css,
      ifelse(hover,
      " .task:hover{
      transition-duration: 0.5s;
      margin: 0%;
      padding: 4%;}",""),
      "</style>")

  # aggregate code components ----
  code <- paste(css, html)
  class(code) <- c("knit_asis", class(code))
  return(code)

}
