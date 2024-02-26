# data manipulation, reading, and writing
library(tidyverse)
library(readxl)
library(GetoptLong)
library(openxlsx)

# plotting
library(ggprism)

# saving survival model parameters
library(broom.mixed)

# lollipop plot
library(ggalt)

# survival analysis
library(survival)
library(ggsurvfit)
library(survminer)


# helper functions
source(file.path("helper_script.R"))

# end of follow-up
max_survival_cutoff_date <- ymd("2024-01-19")

##########################################################################################################################################################################
########################################## BEGIN ANALYSIS ################################################################################################################
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
    # removed for reasons related to BP (taking heart medications, etc) / no baseline SBP measurement
    filter(!`Record ID` %in% c(1, 2, 13, 17))

message("Extracting dates for: ")
baseline_data <- lapply(raw_data %>% split(.$short_measurement), FUN = function(x) {
    message("   ||", x$short_measurement[1])
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
write.xlsx(survival_data_summary, file.path(survival_output_directory_data, "survival_data_with_baselines.xlsx"))

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
write.xlsx(survival_data_prepared_all_cols, file.path(survival_output_directory_data, "canine-processed_survival_data.xlsx"))

# less columns for easy manipulation and viewing
survival_data_prepared <- survival_data_prepared_all_cols %>%
    dplyr::select(record_id, last_drug, total_survival_time, final_status)

# Lollipop plot to see the patients by survival
loli_dat_temp <- survival_data_prepared %>%
    arrange(total_survival_time)

loli_dat <- loli_dat_temp %>%
    mutate(record_id_total_surv_ordered_fct = factor(record_id, levels = unique(loli_dat_temp$record_id))) %>%
    arrange(record_id_total_surv_ordered_fct)

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
write.xlsx(survival_model, file.path(survival_output_directory_data, qq("canine-survival_model_params-all_treatments.xlsx")))

# Plot the survival curves
line_types <- c("dotdash", "solid", "solid")
names(line_types) <- c("last_drug=toceranib", "last_drug=toc + dox", "last_drug=toc + lis")
surv_ggplot <- survminer::ggsurvplot(
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


surv_ggplot


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
    file.path(survival_output_directory_plots, qq("canine-survival_plots-all_treatments.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(surv_ggplot)
dev.off()

pdf(
    file.path(survival_output_directory_plots, qq("canine-survival_plot-toc_vs_lis.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(toc_vs_lis_separated)
dev.off()


pdf(
    file.path(survival_output_directory_plots, qq("canine-survival_plot-toc_vs_dox.pdf")),
    width = my_width, height = my_height,
    onefile = FALSE
)
print(toc_vs_dox_separated)
dev.off()
