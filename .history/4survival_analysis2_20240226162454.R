rm(list = ls())
library(tidyverse)
library(readxl)
# library(GetoptLong)
# library(lmerTest)
# library(broom.mixed)
# library(ggprism)
# library(openxlsx)
# library(survival)
# library(survminer)
# library(ggalt)
# library(ggsurvfit)

data_file <- file.path("data/ToceranibTherapyInDo_DATA_LABELSv3.xlsx")

# end of follow-up
max_survival_cutoff_date <- ymd("2024-01-19")



##########################################################################################################################################################################
##########################################################################################################################################################################

survival_output_directory <- "survival_results"
survival_output_directory_plots <- file.path(survival_output_directory, "plots")
survival_output_directory_data <- file.path(survival_output_directory, "data")
dir.create(survival_output_directory_plots, showWarnings = FALSE, recursive = TRUE)
dir.create(survival_output_directory_data, showWarnings = FALSE, recursive = TRUE)

raw_data <- read_excel(file.path("data", "all_data_combined-processed.xlsx")) %>%
    arrange(`Record ID`) %>%
    mutate(record_id = factor(record_id, levels = unique(record_id))) %>%
    group_by(record_id) %>%
    mutate(
        drug = factor(drug, levels = c("toceranib", "toc + dox", "toc + lis")),
        last_drug = factor(last(drug), levels = c("toceranib", "toc + dox", "toc + lis"))
    ) %>%
    ungroup() %>%
    # removed for reasons related to BP / no baseline SBP measurement
    filter(!`Record ID` %in% c(1, 2, 13, 17))

message("Extracting dates for: ")
baseline_data <- lapply(raw_data %>% split(.$short_measurement), FUN = function(x) {
    message("   ||", x$short_measurement[1])
    # GUARANTEE THAT YOU ARE LOOKING AT THE VERY FIRST AND THE VERY LAST DATE
    # INTENTION TO TREAT - ALL DOGS USE THE LAST DRUG THEY WERE GIVEN
    x %>%
        distinct(short_measurement, `Record ID`, record_id, Date, drug, last_drug, final_htn_group) %>%
        group_by(record_id, `Record ID`) %>% # , drug
        reframe(
            short_measurement = unique(short_measurement),
            Date = Date,
            first_date = as_date(first(Date, order_by = Date)),
            last_drug = unique(last_drug),
            final_htn_group = unique(final_htn_group),
            last_date = as_date(last(Date, order_by = Date)), .groups = "drop"
        )
}) %>%
    bind_rows() %>%
    group_by(record_id, `Record ID`) %>% # , drug
    summarize(
        baseline_date = first(first_date, order_by = first_date),
        absolute_last_visit_date = last(last_date, order_by = last_date),
        final_htn_group = unique(final_htn_group),
        # drug = unique(drug),
        time_to_last_visit = absolute_last_visit_date - baseline_date,
        last_drug = last(last_drug), .groups = "keep"
    ) %>%
    ungroup() %>%
    dplyr::select(-`Record ID`) %>%
    arrange(record_id)
message("Ensured that we are looking at the very first and the very last date.")

# baseline_data %>%
#     print(n = Inf)

survival_data <- read_excel("data/2024-01-19 Canine Patient Survival Data.xlsx") %>%
    mutate(record_id = str_c(`Patient Name`, "-", `Record ID`)) %>%
    rename(Reason = `...5`) %>%
    mutate(
        extracted_date = str_replace(string = str_trim(str_extract(string = Status, pattern = "[0-9]*/[0-9]*(/| )*[0-9]*")), pattern = " ", replacement = "/"),
        extracted_date = as_date(mdy(extracted_date)),
        extracted_status = tolower(str_extract(string = Status, pattern = "Alive|Died|Still")),
        extracted_status = if_else(extracted_status == "still", "alive", extracted_status),
        extracted_reason = str_extract(string = Status, pattern = "cancer"),
        extracted_reason = if_else(!is.na(extracted_reason) & extracted_status == "died", extracted_reason,
            if_else(is.na(extracted_reason) & extracted_status == "died", "unknown", NA_character_)
        ),
    ) %>%
    replace_na(list(extracted_reason = "alive or censored")) %>%
    # right join to just include patients that we analyzed
    right_join(baseline_data,
        by = join_by(record_id)
    ) %>%
    dplyr::select(
        record_id, last_drug, baseline_date, time_to_last_visit, absolute_last_visit_date,
        extracted_date, extracted_status, extracted_reason, final_htn_group
    )

survival_data_summary <- survival_data %>%
    group_by(record_id) %>%
    mutate(
        # Use absolute_last_visit_date directly for stop_date calculation, as it's more relevant for treatment observation
        last_known_alive_date = max(c(extracted_date, absolute_last_visit_date)), # absolute_last_visit_date,
        cutoff_date = max_survival_cutoff_date,
        stop_date = if_else(
            !(is.na(last_known_alive_date) | is.na(extracted_date)),
            last_known_alive_date,
            cutoff_date
        )
    ) %>%
    ungroup()

# survival_data_summary %>%
#     print(n = Inf)

survival_data_prepared_all_cols <- survival_data_summary %>%
    mutate(
        total_survival_time = as.numeric(stop_date - baseline_date),
        # Determine final status: 1 for deceased by the stop_date, 0 for alive or lost to follow-up
        final_status = if_else(
            extracted_status == "died" & !is.na(stop_date),
            1,
            0 # right sensored
        )
    )

survival_data_prepared <- survival_data_prepared_all_cols %>%
    dplyr::select(record_id, last_drug, total_survival_time, final_status)

# Lollipop plot to see the patients by survival
loli_dat_temp <- survival_data_prepared %>%
    arrange(total_survival_time)

loli_dat <- loli_dat_temp %>%
    mutate(record_id_total_surv_ordered_fct = factor(record_id, levels = unique(loli_dat_temp$record_id))) %>%
    arrange(record_id_total_surv_ordered_fct)

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

# Lollipop
loli_surv_gg <- ggplot(loli_dat) +
    ggalt::geom_lollipop(
        mapping = aes(
            x = total_survival_time,
            y = record_id_total_surv_ordered_fct,
            color = last_drug
        ),
        horizontal = TRUE,
        point.size = rel(3.75), size = rel(1.6)
    ) +
    my_pptx_theme +
    scale_color_manual(values = point_colors) +
    theme(
        axis.text.x = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(1)),
        plot.title = element_text(size = rel(1.5))
    ) +
    xlab("Days") +
    ylab("Subject") +
    ggtitle("Total Survival Time")
loli_surv_gg
ggsave(
    filename = file.path(survival_output_directory_plots, "survival_lollipop.pdf"),
    plot = loli_surv_gg,
    width = 9, height = 9
)
write.xlsx(survival_data_summary, file.path(survival_output_directory_data, "survival_data_with_baselines.xlsx"))
# glimpse(survival_data_prepared)
# survival_data_prepared %>%
#     print(n = Inf)
write.xlsx(survival_data_prepared_all_cols, file.path(survival_output_directory_data, "canine-processed_survival_data.xlsx"))
survival_data_prepared_all_cols

# Plot survival curves stratified by drug

#' @note: Method
#' "Survival analysis was conducted on canine patients treated with different drugs.
#' Data cleaning involved grouping records by patient ID to ensure a single baseline
#' measurement per patient, excluding records without baseline values. For each patient,
#' the analysis considered the earliest recorded date as the baseline. Dates of death
#' were extracted from the status updates, and a comprehensive dataset was created by
#' joining this with baseline treatment information. Survival time was calculated from
#' the baseline to either the date of death or the last known alive date, with patients
#' last seen before the analysis cutoff date (the latest date in the dataset) censored
#' to account for lost follow-up.

# Create survival object using the survival package
survival_data_prepared_km_fit <- ggsurvfit::survfit2(
    survival::Surv(time = total_survival_time, event = final_status) ~ last_drug,
    data = survival_data_prepared
)

summary(survival_data_prepared_km_fit)
survival_model <- broom.mixed::tidy(survival_data_prepared_km_fit)
write.xlsx(survival_model, file.path(survival_output_directory_data, qq("@{basename(top_output_dir)}-canine-survival_model_params-all_treatments.xlsx")))
line_types <- c("dotdash", "solid", "solid")
names(line_types) <- c("last_drug=toceranib", "last_drug=toc + dox", "last_drug=toc + lis")

# * When should you look at the results for the test for trend? *
# The test for trend is only relevant when the order of groups (defined by data set columns in Prism) is logical.
# Examples would be if the groups are different age groups, different disease severities, or different doses of a drug.
# The left-to-right order of data sets in Prism must correspond to equally spaced ordered categories.
# If the data sets are not ordered (or not equally spaced), then you should ignore the results of the logrank test for trend.
surv_ggplot <- ggsurvplot(
    fit = survival_data_prepared_km_fit,
    data = survival_data_prepared,
    pval = FALSE,
    conf.int = FALSE,
    risk.table = FALSE,
    test.for.trend = FALSE, # the groups aren't ordered logically, like age groups, so we don't use this
    pval.method = TRUE,
    cumevents = FALSE, # turn off the cumevents table
    risk.table.y.text = FALSE, # show bars instead of names in text annotations
    cumevents.y.text = FALSE, # show bars instead of names in text annotations
    ncensor.plot = FALSE, # plot the number of censored subjects at time t
    surv.median.line = "v", # add the median survival pointer.
    risk.table.height = 0.33,
    # legend.labs =
    #     new_fct_levels,
    color = "strata",
    linetype = "strata",
    palette = unname(point_colors),
    ggtheme = surv_theme,
    size = 1.1,
    censor.shape = "|",
    censor.size = 10,
    pval.size = 7.5,
    font.main = c(35, "bold"),
    font.subtitle = c(30, "plain"),
    font.x = c(25, "bold"),
    font.y = c(25, "bold"),
    font.tickslab = c(25, "plain")
) +
    xlab("Time (Days)")

surv_ggplot$plot <- surv_ggplot$plot + labs(
    title = "Canine Cancer Patient Survival",
    subtitle = "Stratified by Treatment Group",
    caption = "Survival outcomes by treatment group, adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug.\nDashed vertical line is median survival time."
) +
    scale_linetype_manual(values = line_types)

# surv_ggplot$table <- surv_ggplot$table + labs(
#     caption = "Survival outcomes by drug, adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug.\nDotted lines are median survival times by strata."
# )
surv_ggplot

# survival_data_prepared_km_fit %>%
#     gtsummary::tbl_survfit(
#         # time variable in the our data is actually in days, so we need to use times = 365.25)
#         times = 365.25,
#         label_header = "**1-year survival (95% CI)**"
#     )
# summary(survival_data_prepared_km_fit,
#     times = 365.25,
#     extend = TRUE
# )

#########################################################################################################
################################# Toceranib only vs randomized ##########################################
#########################################################################################################

new_fct_levels <- c("Toceranib Alone", "Toceranib + Anti-HTN")
anti_htn_factor_survival_data_prepared <- survival_data_prepared %>%
    mutate(drug_group = factor(if_else(last_drug == "toceranib", "Toceranib Alone", "Toceranib + Anti-HTN"),
        levels = new_fct_levels
    ))

line_types11 <- c("dotdash", "solid")
names(line_types11) <- c("drug_group=Toceranib Alone", "drug_group=Toceranib + Anti-HTN")

sample_size_str_df2 <- anti_htn_factor_survival_data_prepared %>%
    group_by(last_drug) %>%
    tally() %>%
    mutate(str = paste0(last_drug, " = ", n))
sample_size_str_cmdb2 <- str_c(paste(sample_size_str_df2$last_drug, sample_size_str_df2$n, sep = " = "), collapse = " || ")

anti_htn_factor_survival_data_prepared_km_fit <- ggsurvfit::survfit2(
    Surv(total_survival_time, final_status) ~ drug_group,
    data = anti_htn_factor_survival_data_prepared
)

# survdiff(
#     survival::Surv(time = total_survival_time, event = final_status) ~ drug_group,
#     data = anti_htn_factor_survival_data_prepared, rho = 1
# )

summary(anti_htn_factor_survival_data_prepared_km_fit)
survival_model2 <- broom.mixed::tidy(anti_htn_factor_survival_data_prepared_km_fit)
write.xlsx(survival_model2, file.path(survival_output_directory_data, qq("@{basename(top_output_dir)}-canine-survival_model_params-grouped_anti_htn.xlsx")))

point_colors_new <- c("Toceranib Alone" = toceranib_point_color, "Toceranib + Anti-HTN" = "#b516e9")

surv_ggplot_antihtn <- ggsurvplot(
    fit = anti_htn_factor_survival_data_prepared_km_fit,
    data = anti_htn_factor_survival_data_prepared,
    pval = TRUE,
    conf.int = FALSE,
    risk.table = FALSE,
    pval.method = TRUE,
    cumevents = FALSE, # turn off the cumevents table
    color = "strata",
    linetype = "strata",
    risk.table.y.text = FALSE, # show bars instead of names in text annotations
    cumevents.y.text = FALSE, # show bars instead of names in text annotations
    ncensor.plot = FALSE, # plot the number of censored subjects at time t
    surv.median.line = "hv", # add the median survival pointer.
    risk.table.height = 0.33,
    # legend.labs =
    #     new_fct_levels,
    palette = unname(point_colors_new),
    ggtheme = surv_theme,
    size = 1.1,
    censor.shape = "|",
    censor.size = 10,
    pval.size = 7.5,
    font.main = c(35, "bold"),
    font.subtitle = c(30, "plain"),
    font.x = c(25, "bold"),
    font.y = c(25, "bold"),
    font.tickslab = c(25, "plain")
) + xlab("Time (Days)")
# patchwork::plot_annotation(
#     title = "Canine Cancer Patient Survival Stratified by Drug",
#     caption = "Survival outcomes by drug, adjusted for follow-up status. ITT."
# )
surv_ggplot_antihtn$plot <- surv_ggplot_antihtn$plot + labs(
    title = "Canine Cancer Patient Survival",
    subtitle = "Stratified by Treatment Category",
    caption = qq("@{sample_size_str_cmdb2}\nSurvival outcomes by drug category, adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug category.\nDashed lines are median survival times by strata.")
) +
    scale_linetype_manual(values = line_types11)
# surv_ggplot_antihtn$table <- surv_ggplot_antihtn$table + labs(
#     caption = "Survival outcomes by drug category, adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug category.\nDotted lines are median survival times by strata."
# )
# # Labels for Survival Curves (plot)
# surv_ggplot_antihtn$cumevents <- surv_ggplot_antihtn$cumevents + labs(
#     caption = "Survival outcomes by drug, adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug category."
# )
surv_ggplot_antihtn


#########################################################################################################
################################# Dox vs Lis ############################################################
#########################################################################################################


anti_htn_separated_survival_data_prepared <- survival_data_prepared %>%
    filter(last_drug != "toceranib")

anti_htn_separated_survival_data_prepared_km_fit <- survfit(
    Surv(time = total_survival_time, event = final_status) ~ last_drug,
    data = anti_htn_separated_survival_data_prepared
)

survival_model3 <- broom.mixed::tidy(anti_htn_separated_survival_data_prepared_km_fit)
write.xlsx(survival_model3, file.path(survival_output_directory_data, qq("@{basename(top_output_dir)}-canine-survival_model_params-dox_vs_lis.xlsx")))

sample_size_str_df <- anti_htn_separated_survival_data_prepared %>%
    group_by(last_drug) %>%
    tally() %>%
    mutate(str = paste0(last_drug, " = ", n))
sample_size_str_cmdb <- str_c(paste(sample_size_str_df$last_drug, sample_size_str_df$n, sep = " = "), collapse = " || ")

surv_ggplot_antihtn_separated <- ggsurvplot(
    fit = anti_htn_separated_survival_data_prepared_km_fit,
    data = anti_htn_separated_survival_data_prepared,
    pval = TRUE,
    conf.int = FALSE,
    risk.table = FALSE,
    pval.method = TRUE,
    cumevents = FALSE, # turn off the cumevents table
    color = "strata",
    risk.table.y.text = FALSE, # show bars instead of names in text annotations
    cumevents.y.text = FALSE, # show bars instead of names in text annotations
    ncensor.plot = FALSE, # plot the number of censored subjects at time t
    surv.median.line = "hv", # add the median survival pointer.
    risk.table.height = 0.33,
    xlim = c(0, 800),
    break.time.by = 200,
    # legend.labs =
    #     new_fct_levels,
    palette = unname(point_colors[c(2, 3)]),
    ggtheme = surv_theme,
    size = 1.1,
    censor.shape = "|",
    censor.size = 10,
    pval.size = 7.5,
    font.main = c(35, "bold"),
    font.subtitle = c(30, "plain"),
    font.x = c(25, "bold"),
    font.y = c(25, "bold"),
    font.tickslab = c(25, "plain")
) + xlab("Time (Days)")
# patchwork::plot_annotation(
#     title = "Canine Cancer Patient Survival Stratified by Drug",
#     caption = "Survival outcomes by drug, adjusted for follow-up status. ITT."
# )
surv_ggplot_antihtn_separated$plot <- surv_ggplot_antihtn_separated$plot +
    labs(
        title = "Canine Cancer Patient Survival",
        subtitle = "Stratified by Anti-HTN Treatment",
        caption = qq("n = @{sample_size_str_cmdb}\nSurvival outcomes by drug category (Only Anti-HTN group), adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug.\nDashed lines are median survival times by strata.")
    )
# surv_ggplot_antihtn_separated$table <- surv_ggplot_antihtn_separated$table + labs(
#     caption = "Survival outcomes by drug category (Only Anti-HTN group), adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug.\nDotted lines are median survival times by strata."
# )
surv_ggplot_antihtn_separated



#########################################################################################################
################################# Toceranib vs Lis ######################################################
#########################################################################################################



toc_vs_lis_df <- survival_data_prepared %>%
    filter(last_drug != "toc + dox")

line_types2 <- c("dotdash", "solid")
names(line_types2) <- c("last_drug=toceranib", "last_drug=toc + lis")

toc_vs_lis_df_km_fit <- survfit(
    Surv(time = total_survival_time, event = final_status) ~ last_drug,
    data = toc_vs_lis_df
)

survival_model4 <- broom.mixed::tidy(toc_vs_lis_df_km_fit)
write.xlsx(survival_model4, file.path(survival_output_directory_data, qq("@{basename(top_output_dir)}-canine-survival_model_params-toc_vs_lis.xlsx")))

sample_size_str_df <- toc_vs_lis_df %>%
    group_by(last_drug) %>%
    tally() %>%
    mutate(str = paste0(last_drug, " = ", n))
sample_size_str_cmdb <- str_c(paste(sample_size_str_df$last_drug, sample_size_str_df$n, sep = " = "), collapse = " || ")

toc_vs_lis_separated <- ggsurvplot(
    fit = toc_vs_lis_df_km_fit,
    data = toc_vs_lis_df,
    pval = TRUE,
    conf.int = FALSE,
    risk.table = FALSE,
    pval.method = TRUE,
    cumevents = FALSE, # turn off the cumevents table
    color = "strata",
    linetype = "strata",
    risk.table.y.text = FALSE, # show bars instead of names in text annotations
    cumevents.y.text = FALSE, # show bars instead of names in text annotations
    ncensor.plot = FALSE, # plot the number of censored subjects at time t
    surv.median.line = "hv", # add the median survival pointer.
    risk.table.height = 0.33,
    # legend.labs =
    #     new_fct_levels,
    palette = unname(point_colors[c(1, 3)]),
    ggtheme = surv_theme,
    size = 1.1,
    censor.shape = "|",
    censor.size = 10,
    pval.size = 7.5,
    font.main = c(35, "bold"),
    font.subtitle = c(30, "plain"),
    font.x = c(25, "bold"),
    font.y = c(25, "bold"),
    font.tickslab = c(25, "plain")
) + xlab("Time (Days)")
# patchwork::plot_annotation(
#     title = "Canine Cancer Patient Survival Stratified by Drug",
#     caption = "Survival outcomes by drug, adjusted for follow-up status. ITT."
# )
toc_vs_lis_separated$plot <- toc_vs_lis_separated$plot + labs(
    title = "Canine Cancer Patient Survival",
    subtitle = "Toceranib Only vs Toc + Lis",
    caption = qq("n = @{sample_size_str_cmdb}\nSurvival outcomes Toc vs Toc + Lis, adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug.\nDashed lines are median survival times by strata.")
) +
    scale_linetype_manual(values = line_types2)

toc_vs_lis_separated



#########################################################################################################
################################# Toceranib vs Dox ######################################################
#########################################################################################################


toc_vs_dox_df <- survival_data_prepared %>%
    filter(last_drug != "toc + lis")

line_types3 <- c("dotdash", "solid")
names(line_types3) <- c("last_drug=toceranib", "last_drug=toc + dox")

toc_vs_dox_df_km_fit <- survfit(
    Surv(time = total_survival_time, event = final_status) ~ last_drug,
    data = toc_vs_dox_df
)

survival_model5 <- broom.mixed::tidy(toc_vs_dox_df_km_fit)
write.xlsx(survival_model5, file.path(survival_output_directory_data, qq("@{basename(top_output_dir)}-canine-survival_model_params-toc_vs_dox.xlsx")))

sample_size_str_df <- toc_vs_dox_df %>%
    group_by(last_drug) %>%
    tally() %>%
    mutate(str = paste0(last_drug, " = ", n))
sample_size_str_cmdb <- str_c(paste(sample_size_str_df$last_drug, sample_size_str_df$n, sep = " = "), collapse = " || ")

toc_vs_dox_separated <- ggsurvplot(
    fit = toc_vs_dox_df_km_fit,
    data = toc_vs_dox_df,
    pval = TRUE,
    conf.int = FALSE,
    risk.table = FALSE,
    pval.method = TRUE,
    cumevents = FALSE, # turn off the cumevents table
    color = "strata",
    linetype = "strata",
    risk.table.y.text = FALSE, # show bars instead of names in text annotations
    cumevents.y.text = FALSE, # show bars instead of names in text annotations
    ncensor.plot = FALSE, # plot the number of censored subjects at time t
    surv.median.line = "hv", # add the median survival pointer.
    risk.table.height = 0.33,
    # legend.labs =
    #     new_fct_levels,
    palette = unname(point_colors[c(1, 2)]),
    ggtheme = surv_theme,
    size = 1.1,
    censor.shape = "|",
    censor.size = 10,
    pval.size = 7.5,
    font.main = c(35, "bold"),
    font.subtitle = c(30, "plain"),
    font.x = c(25, "bold"),
    font.y = c(25, "bold"),
    font.tickslab = c(25, "plain")
) + xlab("Time (Days)")
# patchwork::plot_annotation(
#     title = "Canine Cancer Patient Survival Stratified by Drug",
#     caption = "Survival outcomes by drug, adjusted for follow-up status. ITT."
# )
toc_vs_dox_separated$plot <- toc_vs_dox_separated$plot + labs(
    title = "Canine Cancer Patient Survival",
    subtitle = "Toceranib Only vs Toc + Dox",
    caption = qq("n = @{sample_size_str_cmdb}\nSurvival outcomes Toc vs Toc + Dox, adjusted for follow-up status.\nIntention-to-treat grouping.\nBased on Kaplan-Meier estimate of survival time for each drug.\nDashed lines are median survival times by strata.")
) +
    scale_linetype_manual(values = line_types3)

toc_vs_dox_separated




my_width <- 12
my_height <- 11.5
pdf(
    file.path(survival_output_directory_plots, qq("@{basename(top_output_dir)}-canine-survival_plots-all_treatments.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(surv_ggplot)
dev.off()

pdf(
    file.path(survival_output_directory_plots, qq("@{basename(top_output_dir)}-canine-survival_plot-grouped_anti_htn.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(surv_ggplot_antihtn)
dev.off()

pdf(
    file.path(survival_output_directory_plots, qq("@{basename(top_output_dir)}-canine-survival_plot-dox_vs_lis.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(surv_ggplot_antihtn_separated)
dev.off()

pdf(
    file.path(survival_output_directory_plots, qq("@{basename(top_output_dir)}-canine-survival_plot-toc_vs_lis.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(toc_vs_lis_separated)
dev.off()


pdf(
    file.path(survival_output_directory_plots, qq("@{basename(top_output_dir)}-canine-survival_plot-toc_vs_dox.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(toc_vs_dox_separated)
dev.off()



### COX REGRESSION ?????

# Fitting a Cox model with time-varying covariates
# Assuming survival_data_long has columns: id (for patient ID), start_time, stop_time, event (1 if the event occurred, 0 otherwise), treatment (as a time-varying covariate)
# glimpse(survival_data_prepared_censored)

# https://www.drizopoulos.com/courses/emc/basic_surivival_analysis_in_r

#' The function that fits Cox models from the survival package is coxph(). The
#' Cox model is a semi-parametric model that does not assume a particular distribution
#' for the survival times).

# cox_surv_data <- survival_data_prepared_censored %>%
#     group_by(record_id) %>%
#     mutate(num_recs = max(n()), internal_index = dense_rank(baseline_date)) %>%
#     mutate(
#         start = if_else(num_recs == 1, 0,
#             if_else(internal_index == 1, 0, lag(surv_time, default = first(surv_time)))
#         ),
#         end = if_else(num_recs == 1, surv_time,
#             if_else(internal_index == 1,
#                 first(surv_time),
#                 first(surv_time, default = first(surv_time)) + lead(surv_time, default = last(surv_time))
#             )
#         ),
#     ) %>%
#     dplyr::select(record_id, drug, start, end, total_survival_time, status_num)


# cox_joint_model <- coxph(Surv(start, end, status_num) ~ drug * total_survival_time,
#     data = cox_surv_data
# )
# ggforest(cox_joint_model, data = cox_surv_data)
# summary(cox_joint_model)
# #' Coefficients Table
# #'
# #' coef (Coefficient): The estimated log hazard ratio for each level of the drug variable compared to the baseline (reference) category.
# #' A positive coefficient indicates a higher hazard (risk) of the event occurring, and a negative coefficient indicates a lower hazard.
# #' drugtoc + dox: The coefficient for "toc + dox" indicates its hazard ratio relative to the baseline drug treatment (assumed to be "toceranib"
# #' if it's the omitted category). A coefficient of 0.8152 suggests a higher hazard compared to the baseline.
# #' drugtoc + lis: The coefficient for "toc + lis" is -1.4204, suggesting a lower hazard relative to the baseline.
# #'
# #' exp(coef) (Hazard Ratio): The exponentiation of the coefficient, providing the multiplicative effect on the hazard for a one-unit increase in
# #'  the predictor. Values above 1 indicate an increased hazard, while values below 1 indicate a decreased hazard.
# #' "toc + dox" has a hazard ratio of 2.2596, suggesting that being on this drug treatment more than doubles the hazard compared to the baseline.
# #' "toc + lis" has a hazard ratio of 0.2416, suggesting a substantial reduction in hazard.
# #' robust se (Robust Standard Error): The standard error of the coefficient estimates, adjusted for the clustering.
# #'
# #' z: The z-statistic, calculated as the coefficient divided by its robust standard error.
# #'
# #' Pr(>|z|): The p-value associated with the z-statistic, testing the null hypothesis that the coefficient (log hazard ratio) is zero. Values below
# #' 0.05 suggest significant differences from the baseline category.
# #'

# # Estimate survival curves from the Cox model
# # cox_joint_surv_fit <- surv_fit(cox_joint_model, data = cox_surv_data)
# # surv_pvalue(cox_joint_surv_fit)

# # Now, you can plot this using ggsurvplot
# ggsurvplot(
#     fit = survfit(cox_joint_model),
#     data = cox_surv_data,
#     pval = TRUE,
#     conf.int = TRUE,
#     risk.table = TRUE, # Show the number at risk table below the plot
#     pval.method = TRUE, # Show the p-value of the log-rank test
#     palette = "Dark2", # Color palette
#     ggtheme = theme_minimal()
# ) # ggplot2 theme
