# Custom theme for data visualizations
plot_theme <- function(
    title_size = NULL, xaxis_size = NULL, 
    yaxis_size = NULL, strip_size = NULL, 
    strip_face = NULL, caption.hjust = 1, 
    caption.vjust = 0, x_axis_face = NULL, 
    y_axis_face = NULL, transparent = FALSE, 
    axis_text_size = NULL, 
    legend_text_size = NULL,
    subtitle_size = NULL,
    caption_size = NULL,
    ...
) {
  .theme <- theme_minimal() + theme(
    # Set the outer margins of the plot to 1/5 of an inch on all sides
    #plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"),
    # Specify the default settings for the plot title
    plot.title = element_text(
      size = title_size,
      face = "bold",
      family = "serif"
    ),
    # Specify the default settings for caption text
    plot.caption = element_text(
      size = caption_size,
      family = "serif",
      hjust = caption.hjust,
      vjust = caption.vjust
    ),
    # Specify the default settings for subtitle text
    plot.subtitle = element_text(
      size = subtitle_size,
      family = "serif"
    ),
    # Specify the default settings specific to the x axis title
    axis.title.y = element_text(
      size = yaxis_size, 
      face = "bold", 
      family = "serif",
      margin = margin(r = 10, l = -10)
    ),
    # Specify the default settings specific to the y axis title
    axis.title.x = element_text(
      size = xaxis_size, 
      face = "bold", 
      family = "serif",
      margin = margin(t = 10, b = -10)
    ),
    # Specify the default settings for x axis text
    axis.text.x = element_text(
      size = axis_text_size,
      family = "serif",
      face = x_axis_face
    ),
    # Specify the default settings for y axis text
    axis.text.y = element_text(
      size = axis_text_size,
      family = "serif",
      face = y_axis_face
    ),
    # Specify the default settings for legend titles
    legend.title = element_text(
      size = legend_text_size,
      face = "bold",
      family = "serif"
    ),
    # Specify the default settings for legend text
    legend.text = element_text(
      size = legend_text_size,
      family = "serif"
    ),
    #
    strip.background = element_rect(fill = NA),
    #
    strip.text = element_text(
      family = "serif", 
      size = strip_size,
      face = strip_face
    ),
    # Additional Settings Passed to theme()
    ...
  )
  # Plot Transparency
  if (transparent == TRUE) {
    .theme <- .theme + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      #
      plot.background = element_rect(fill = "transparent", colour = NA),
      #
      legend.background = element_rect(fill = "transparent", colour = NA),
      #
      legend.key = element_rect(fill = "transparent", colour = NA)
    )
  }
  return(.theme)
}