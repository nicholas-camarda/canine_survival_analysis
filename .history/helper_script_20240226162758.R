# Theme
my_base_pptx_size <- 16
my_pptx_theme <- theme_prism(base_size = my_base_pptx_size) +
    theme(
        strip.text.x = element_text(size = rel(1), face = "bold"),
        # axis.text.x = element_text(size = rel(0.75)), # , hjust = 1, vjust = 1), # angle = 45,
        panel.grid.major = element_line(colour = "gray", linetype = 3, linewidth = 0.5),
        panel.grid.minor = element_line(colour = "gray", linetype = 2, linewidth = 0.25),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.6)),
        axis.text.x = element_text(size = rel(0.9), angle = 45, hjust = 1, vjust = 1),
        axis.ticks = element_line(linewidth = rel(1.5)),
        axis.line = element_line(linewidth = rel(1.75))
    )

surv_theme <- theme_prism(base_size = 20) +
    theme(
        panel.grid.major = element_line(colour = "gray", linetype = 3, linewidth = 0.5),
        panel.grid.minor = element_line(colour = "gray", linetype = 2, linewidth = 0.25),
        legend.position = "bottom",
        axis.text = element_text(size = rel(0.75)),
        plot.subtitle = element_text(size = rel(0.85)),
        plot.caption = element_text(size = rel(0.75))
    )


# Plot survival curves stratified by drug
toceranib_line_color <- "#d4d4d4"
toceranib_point_color <- "#535252"

toc_dox_line_color <- "#CCD7E9"
toc_dox_point_color <- "#9BB3D3"

toc_lis_line_color <- "#E3938A"
toc_lis_point_color <- "#B53530"

line_colors <- c("toceranib" = toceranib_line_color, "toc + dox" = toc_dox_line_color, "toc + lis" = toc_lis_line_color)
point_colors <- c("toceranib" = toceranib_point_color, "toc + dox" = toc_dox_point_color, "toc + lis" = toc_lis_point_color)
point_shapes <- c("toceranib" = 24, "toc + dox" = 21, "toc + lis" = 23)
