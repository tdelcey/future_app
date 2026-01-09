# utils/ui_helpers.R

callout_box <- function(
  title,
  text,
  border = "#1976D2",
  bg = "#E3F2FD",
  icon = "\u2139", # ℹ par défaut
  icon_color = "#0D47A1",
  icon_size = "18px"
) {
  icon_html <- tags$span(
    icon,
    style = sprintf(
      "
        font-size:%s;
        font-weight:bold;
        color:%s;
        margin-right:8px;
        vertical-align:middle;
      ",
      icon_size,
      icon_color
    )
  )

  div(
    style = sprintf(
      "
      border-left:4px solid %s;
      background:%s;
      padding:12px 16px;
      margin-bottom:14px;
      border-radius:6px;
      ",
      border,
      bg
    ),

    div(
      style = "
        font-weight:600;
        margin-bottom:6px;
        display:flex;
        align-items:center;
      ",
      icon_html,
      span(title)
    ),

    p(style = "margin:0; line-height:1.4;", text)
  )
}
