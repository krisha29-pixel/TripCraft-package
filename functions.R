# ============================================================
#  TripCraft — Core Functions (Fully Fixed)
#  File: R/functions.R
#
#  Fixes applied:
#  1. geosphere now declared in DESCRIPTION Imports (not just here)
#  2. Vibe toggle buttons use shiny::actionButton to avoid span warning
#  3. Days slider auto-capped with an informative message when
#     available clusters < requested days
# ============================================================

#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import geosphere
#' @import bslib
#' @import scales
NULL

VIBES <- c("Heritage", "Nature", "Adventure", "Food", "Religious", "Shopping")

# ---- 1. Load Data ------------------------------------------

#' Load bundled TripCraft attraction dataset
#' @return data.frame with attraction info for Indian cities
#' @export
tc_load <- function() {
  path <- system.file("extdata", "TripCraft_Data.csv", package = "TripCraft")
  if (path == "" || !file.exists(path)) {
    path <- file.path("inst", "extdata", "TripCraft_Data.csv")
  }
  if (!file.exists(path))
    stop("TripCraft_Data.csv not found. Expected at inst/extdata/TripCraft_Data.csv")
  read.csv(path, stringsAsFactors = FALSE)
}

# ---- 2. Filter City ----------------------------------------

#' Filter attractions by city, budget and vibe
#' @export
tc_filter <- function(df, city, max_fee = Inf, vibes = NULL) {
  d <- df[tolower(df$city) == tolower(city), ]
  
  d$vibe <- dplyr::case_when(
    tolower(d$sub_category) %in% c("heritage", "museum", "architecture", "monument") ~ "Heritage",
    tolower(d$sub_category) %in% c("nature", "viewpoint", "garden", "lake", "beach") ~ "Nature",
    tolower(d$sub_category) %in% c("spiritual", "buddhism", "religious")              ~ "Religious",
    tolower(d$sub_category) %in% c("adventure", "trek")                               ~ "Adventure",
    tolower(d$sub_category) == "shopping" | tolower(d$category) == "market"           ~ "Shopping",
    grepl("food|lassi|chai|dhaba|street|lunch|dinner",
          d$attraction_name, ignore.case = TRUE)                                       ~ "Food",
    TRUE                                                                               ~ "Heritage"
  )
  
  d <- d[d$entry_fee_inr <= max_fee, ]
  if (!is.null(vibes) && length(vibes) > 0) d <- d[d$vibe %in% vibes, ]
  d
}

# ---- 3. Haversine Distance ---------------------------------

#' Great-circle distance in km between two lat/lon points
#' @export
tc_dist <- function(la1, lo1, la2, lo2)
  geosphere::distHaversine(c(lo1, la1), c(lo2, la2)) / 1000

# ---- 4. K-Means Clustering ---------------------------------

#' Group attractions into k geographic day-clusters
#' @export
tc_cluster <- function(d, k = 3) {
  if (nrow(d) == 0) { d$cluster <- integer(0); return(d) }
  if (nrow(d) < k)  { d$cluster <- seq_len(nrow(d)); return(d) }
  set.seed(42)
  d$cluster <- stats::kmeans(d[, c("longitude", "latitude")],
                             centers = k, nstart = 25)$cluster
  d
}

# ---- 5. Build Itinerary ------------------------------------

#' Build a day-wise ordered itinerary from clustered attractions
#' @export
tc_itinerary <- function(cd, days = 3) {
  
  if (!"cluster" %in% names(cd))
    stop("'cd' has no 'cluster' column — run tc_cluster() first.")
  
  if (!nrow(cd)) return(list())
  
  DAY_LABELS <- c("Old city", "Lakes & Forts", "Markets & More", paste0("Day ", 4:7))
  label_for  <- function(i) if (i <= length(DAY_LABELS)) DAY_LABELS[i] else paste("Day", i)
  
  # Cap to available clusters so we never return an empty day
  avail_clusters <- sort(unique(cd$cluster))
  ids <- head(avail_clusters, days)
  
  Filter(Negate(is.null), lapply(seq_along(ids), function(i) {
    d <- cd[cd$cluster == ids[i], ]
    if (nrow(d) == 0L) return(NULL)
    
    d$avg_duration_hrs <- as.numeric(d$avg_duration_hrs)
    
    rem <- seq_len(nrow(d)); cur <- which.max(d$rating)
    ord <- cur; rem <- rem[rem != cur]
    while (length(rem)) {
      nxt <- rem[which.min(sapply(rem, function(j)
        tc_dist(d$latitude[cur], d$longitude[cur], d$latitude[j], d$longitude[j])))]
      ord <- c(ord, nxt); rem <- rem[rem != nxt]; cur <- nxt
    }
    d <- d[ord, ]
    rownames(d) <- NULL
    d$day <- i
    
    leg_indices <- seq_len(nrow(d))[-1]
    d$leg_km <- c(0, sapply(leg_indices, function(r)
      round(tc_dist(d$latitude[r-1], d$longitude[r-1],
                    d$latitude[r],   d$longitude[r]), 1)))
    
    h <- 9; m <- 0
    d$arrive_time <- sapply(seq_len(nrow(d)), function(r) {
      t  <- sprintf("%d:%02d %s", ifelse(h %% 12 == 0, 12, h %% 12), m,
                    ifelse(h < 12, "AM", "PM"))
      dm <- round(d$avg_duration_hrs[r] * 60) + 30
      h  <<- h + (m + dm) %/% 60
      m  <<- (m + dm) %% 60
      t
    })
    d$day_label <- label_for(i)
    d
  }))
}

# ---- 6. Budget Breakdown -----------------------------------

#' Compute trip budget breakdown
#' @export
tc_budget <- function(itin) {
  if (!length(itin)) {
    return(list(entry  = 0, food = 0, shop = 0, total = 0, detail = data.frame()))
  }
  
  s <- do.call(rbind, itin)
  
  if (is.null(s) || nrow(s) == 0) {
    return(list(entry  = 0, food = 0, shop = 0, total = 0, detail = data.frame()))
  }
  
  s$stop_spend <- s$entry_fee_inr +
    ifelse(s$vibe == "Food",     250, 0) +
    ifelse(s$vibe == "Shopping", 1000, 0)
  
  entry <- sum(s$entry_fee_inr)
  food  <- sum(ifelse(s$vibe == "Food", 250, 100))
  shop  <- sum(ifelse(s$vibe == "Shopping", 500, 0))
  
  list(
    entry  = entry,
    food   = food,
    shop   = shop,
    total  = entry + food + shop,
    detail = s[, c("attraction_name", "vibe", "entry_fee_inr", "stop_spend")]
  )
}

# ---- 7. kNN Recommendations --------------------------------

#' Recommend attractions using k-Nearest Neighbours
#' @export
tc_knn <- function(all_d, sel_vibes, exclude = character(0), k = 6) {
  cands <- all_d[!all_d$attraction_name %in% exclude, ]
  if (nrow(cands) < 2) return(data.frame())
  
  feat <- cbind(
    sapply(VIBES, function(v) as.integer(cands$vibe == v)),
    fee = cands$entry_fee_inr / (max(cands$entry_fee_inr) + 1),
    rat = cands$rating / 5
  )
  qrow      <- c(as.integer(VIBES %in% sel_vibes), 0, 1)
  dists     <- apply(feat, 1, function(r) sqrt(sum((r - qrow)^2)))
  cands$sim <- round(pmax(0, 1 - dists / sqrt(length(qrow))) * 100)
  head(cands[order(-cands$sim), ], k)
}

# ---- 8. PCA ------------------------------------------------

#' Run PCA on attraction feature space
#' @export
tc_pca <- function(d) {
  if (nrow(d) < 2) { d$PC1 <- 0; d$PC2 <- 0; return(d) }
  fe <- cbind(
    sapply(VIBES, function(v) as.integer(d$vibe == v)),
    fee = d$entry_fee_inr, rat = d$rating, dur = d$avg_duration_hrs
  )
  fe <- fe[, apply(fe, 2, stats::sd) > 0, drop = FALSE]
  if (ncol(fe) < 2) { d$PC1 <- 0; d$PC2 <- 0; return(d) }
  sc <- stats::prcomp(fe, scale. = TRUE)$x[, 1:2]
  d$PC1 <- sc[, 1]; d$PC2 <- sc[, 2]
  d
}

# ---- 9. Launch Shiny Dashboard -----------------------------

#' Launch the TripCraft interactive Shiny dashboard
#' @return Shiny app object
#' @export
#' @examples
#' \dontrun{ tc_run() }
tc_run <- function() {
  
  VC  <- c(Heritage="#6B70C4", Nature="#4CAF82", Adventure="#E67E22",
           Food="#F0B429", Religious="#E57373", Shopping="#E91E8C")
  DC  <- c("#5B9BD5","#4CAF82","#E07B54","#9C77B8","#F0B429","#E57373","#4DD0E1")
  AV  <- names(VC)
  RAW <- tc_load()
  
  pill <- function(val, lbl, col)
    shiny::div(
      style = sprintf("border-left:4px solid %s;padding:12px 18px;background:#fff;
        border-radius:8px;margin:6px;display:inline-block;min-width:110px;
        box-shadow:0 1px 4px rgba(0,0,0,.06);", col),
      shiny::div(style = "font-size:20px;font-weight:700;", val),
      shiny::div(style = "font-size:11px;color:#888;", lbl))
  
  sbox <- function(val, lbl)
    shiny::div(
      style = "background:#fff;border-radius:10px;padding:11px 14px;margin:4px;
        display:inline-block;min-width:80px;text-align:center;
        box-shadow:0 1px 4px rgba(0,0,0,.07);",
      shiny::div(style = "font-size:20px;font-weight:700;", val),
      shiny::div(style = "font-size:11px;color:#888;margin-top:2px;", lbl))
  
  pthm <- function()
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#F5F4EF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#F5F4EF", colour = NA),
      panel.grid       = ggplot2::element_line(colour = "#E8E8E8"),
      legend.position  = "none")
  
  # ── UI ──────────────────────────────────────────────────────
  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(bg="#F5F4EF", fg="#1a1a1a", primary="#5B9BD5",
                            base_font = bslib::font_google("Inter")),
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      .side  { background:#fff;border-radius:12px;padding:18px;
               box-shadow:0 2px 8px rgba(0,0,0,.06) }
      .scard { background:#fff;border-radius:10px;padding:12px 16px;
               margin-bottom:8px;border-left:4px solid #ccc;
               box-shadow:0 1px 4px rgba(0,0,0,.06) }
      .lbadge{ background:#F0F0F0;border-radius:12px;padding:3px 11px;
               font-size:11px;color:#666;display:inline-block;margin:4px 0 }
      .tbtn  { background:#fff;border:1.5px solid #E0E0E0;border-radius:20px;
               padding:6px 16px;margin:3px;cursor:pointer;
               font-size:13px;font-weight:500 }
      .tbtn.active { background:#1a1a1a;color:#fff;border-color:#1a1a1a }
      label  { font-size:11px;font-weight:600;color:#555;
               letter-spacing:.05em;text-transform:uppercase }
      .irs--shiny .irs-bar   { background:#1a1a1a }
      .irs--shiny .irs-handle{ border-color:#1a1a1a }
      /* FIX: remove default button styling so vibe buttons look custom */
      .vibe-btn { border:none;background:none;padding:0;cursor:pointer; }
    "))),
    shiny::fluidRow(
      shiny::column(3, shiny::div(class = "side",
                                  shiny::selectInput("city", "CITY", sort(unique(RAW$city)), "Jaipur"),
                                  shiny::sliderInput("days", "DAYS", 1, 7, 3, 1),
                                  # FIX: show a message when days exceed available clusters
                                  shiny::uiOutput("days_warning"),
                                  shiny::sliderInput("fee",  "MAX ENTRY FEE (Rs)", 0, 1000, 200, 50),
                                  shiny::tags$label("VIBE"), shiny::tags$br(),
                                  # FIX: vibe buttons now use tags$button inside a div — no span wrapping
                                  shiny::uiOutput("vbtns"),
                                  shiny::sliderInput("k", "DAY CLUSTERS (K)", 1, 7, 3, 1),
                                  shiny::tags$hr(),
                                  shiny::uiOutput("sstats")
      )),
      shiny::column(9,
                    shiny::uiOutput("tabnav"), shiny::tags$br(),
                    shiny::uiOutput("tabcontent"))
    )
  )
  
  # ── Server ──────────────────────────────────────────────────
  server <- function(input, output, session) {
    
    cur  <- shiny::reactiveVal("Journey")
    selv <- shiny::reactiveVal(AV)
    
    # FIX: vibe buttons rebuilt with proper HTML tags (no nested span warning)
    output$vbtns <- shiny::renderUI({
      sv <- selv()
      vs <- list(
        Heritage  = "background:#EEF0FF;color:#6B70C4;border:1.5px solid #6B70C4",
        Nature    = "background:#E8F8F0;color:#4CAF82;border:1.5px solid #4CAF82",
        Adventure = "background:#FFF3E0;color:#E67E22;border:1.5px solid #E67E22",
        Food      = "background:#FFFDE7;color:#F0B429;border:1.5px solid #F0B429",
        Religious = "background:#FFEBEE;color:#E57373;border:1.5px solid #E57373",
        Shopping  = "background:#FCE4EC;color:#E91E8C;border:1.5px solid #E91E8C")
      
      # Use shiny::tags$button directly — avoids the span-inside-button warning
      shiny::div(
        style = "display:flex;flex-wrap:wrap;gap:4px;margin-top:4px;",
        lapply(AV, function(v) {
          active_style <- if (v %in% sv) vs[[v]]
          else "background:#F5F5F5;color:#999;border:1.5px solid #E0E0E0"
          shiny::tags$button(
            type    = "button",
            style   = paste0(active_style,
                             ";border-radius:14px;padding:5px 13px;",
                             "cursor:pointer;font-size:12px;font-weight:500;",
                             "font-family:inherit;"),
            onclick = sprintf(
              "Shiny.setInputValue('vt','%s',{priority:'event'})", v),
            v   # plain text label — no nested tags$span
          )
        })
      )
    })
    
    shiny::observeEvent(input$vt, {
      v <- input$vt; sv <- selv()
      selv(if (v %in% sv) { if (length(sv) > 1) sv[sv != v] else sv } else c(sv, v))
    })
    shiny::observeEvent(input$tc, cur(input$tc))
    
    cd   <- shiny::reactive({
      shiny::req(input$city)
      tc_filter(RAW, input$city, input$fee, selv())
    })
    cda  <- shiny::reactive({
      shiny::req(input$city)
      tc_filter(RAW, input$city, vibes = NULL)
    })
    cl   <- shiny::reactive({
      d <- cd()
      if (nrow(d) == 0) return(d)
      tc_cluster(d, min(input$k, nrow(d)))
    })
    
    # FIX: compute how many days are actually available
    avail_days <- shiny::reactive({
      d <- cl()
      if (nrow(d) == 0) return(0L)
      length(unique(d$cluster))
    })
    
    # FIX: cap requested days to available clusters; show a message when capped
    effective_days <- shiny::reactive({
      min(input$days, avail_days())
    })
    
    output$days_warning <- shiny::renderUI({
      req_days   <- input$days
      avail      <- avail_days()
      if (avail == 0 || req_days <= avail) return(NULL)
      shiny::div(
        style = paste0("background:#FFF8E1;border-left:4px solid #F0B429;",
                       "border-radius:6px;padding:8px 12px;margin:6px 0;",
                       "font-size:12px;color:#795548;"),
        shiny::tags$b("Note: "),
        sprintf(
          "Only %d day%s of attractions available with current filters. ",
          avail, if (avail == 1) "" else "s"),
        "Showing ", avail, if (avail == 1) " day." else " days.",
        shiny::tags$br(),
        shiny::tags$span(
          style = "color:#999;font-size:11px;",
          "Try increasing Max Entry Fee or selecting more Vibes to unlock more days.")
      )
    })
    
    itin <- shiny::reactive({
      d <- cl()
      if (nrow(d) == 0) return(list())
      tc_itinerary(d, effective_days())   # use capped day count
    })
    
    bud <- shiny::reactive({
      tc_budget(itin())
    })
    
    pcd  <- shiny::reactive({
      d <- cd()
      if (nrow(d) < 2) return(d)
      tc_pca(d)
    })
    recs <- shiny::reactive({
      it <- itin()
      if (!length(it)) return(data.frame())
      tc_knn(cda(), selv(), do.call(rbind, it)$attraction_name)
    })
    
    output$sstats <- shiny::renderUI({
      it <- itin(); if (!length(it)) return(NULL)
      s  <- do.call(rbind, it); km <- round(sum(s$leg_km, na.rm = TRUE), 1)
      shiny::tagList(
        sbox(km, "Total km"),
        sbox(nrow(s), "Stops"),
        sbox(round(km / max(input$days, 1), 1), "Avg km/day"),
        sbox(paste0("Rs", bud()$total), "Est. total"))
    })
    
    output$tabnav <- shiny::renderUI({
      shiny::tagList(lapply(
        c("Journey","Route map","PCA explorer","For you","Budget"), function(t)
          shiny::tags$button(t,
                             type    = "button",
                             class   = if (t == cur()) "tbtn active" else "tbtn",
                             onclick = sprintf("Shiny.setInputValue('tc','%s',{priority:'event'})", t))))
    })
    
    output$tabcontent <- shiny::renderUI({
      switch(cur(),
             "Journey"      = j_ui(),
             "Route map"    = r_ui(),
             "PCA explorer" = p_ui(),
             "For you"      = f_ui(),
             "Budget"       = b_ui())
    })
    
    # ── Journey ───────────────────────────────────────────────
    j_ui <- function() {
      it <- itin()
      if (!length(it)) return(shiny::p("No attractions match. Try relaxing filters or increasing Max Entry Fee."))
      s  <- do.call(rbind, it); km <- round(sum(s$leg_km, na.rm = TRUE), 1)
      shiny::tagList(
        shiny::div(
          pill(nrow(s),                  "Total stops",    "#6B70C4"),
          pill(effective_days(),         "Days",           "#4CAF82"),
          pill(paste0(km, " km"),        "Total distance", "#5B9BD5"),
          pill(paste0("Rs",bud()$total), "Est. spend",     "#E91E8C")),
        shiny::tags$br(),
        shiny::tagList(lapply(seq_along(it), function(di) {
          d <- it[[di]]; dc <- DC[((di-1) %% 7) + 1]
          shiny::div(
            shiny::div(
              style = sprintf("background:%s22;border-radius:20px;
                display:inline-block;padding:5px 15px;margin:8px 0;", dc),
              shiny::tags$span(
                style = sprintf("color:%s;font-weight:700;", dc),
                sprintf("\u25cf Day %d \u2014 %s", di, d$day_label[1])),
              shiny::tags$span(
                style = "color:#666;font-size:12px;margin-left:8px;",
                sprintf("%d stops \u00b7 %s km", nrow(d),
                        round(sum(d$leg_km, na.rm=TRUE), 1)))),
            shiny::tagList(lapply(seq_len(nrow(d)), function(si) {
              r   <- d[si, ]; vc <- VC[r$vibe]; if (is.na(vc)) vc <- "#999"
              fee <- if (r$entry_fee_inr == 0) "Free entry" else paste0("Rs", r$entry_fee_inr)
              shiny::tagList(
                if (si > 1 && !is.na(r$leg_km) && r$leg_km > 0)
                  shiny::div(class="lbadge", sprintf("\u25cf %s km", r$leg_km)),
                shiny::div(class="scard",
                           style=sprintf("border-left-color:%s;", vc),
                           shiny::div(style="display:flex;justify-content:space-between;",
                                      shiny::div(
                                        shiny::div(style="font-size:15px;font-weight:600;", r$attraction_name),
                                        shiny::div(style="font-size:12px;color:#888;",
                                                   sprintf("%s \u00b7 %s hrs", r$arrive_time, r$avg_duration_hrs))),
                                      shiny::div(style="text-align:right;",
                                                 shiny::div(style="font-size:13px;font-weight:600;",
                                                            sprintf("%.1f \u2605", r$rating)),
                                                 shiny::div(style="font-size:12px;color:#888;", fee))),
                           shiny::div(
                             style = sprintf(
                               "background:%s22;color:%s;border-radius:12px;
                       padding:2px 10px;font-size:11px;display:inline-block;margin-top:4px;",
                               vc, vc),
                             r$vibe))
              )
            }))
          )
        }))
      )
    }
    
    # ── Route Map ─────────────────────────────────────────────
    r_ui <- function() {
      it <- itin()
      if (!length(it)) return(shiny::p("No attractions match. Try relaxing filters."))
      shiny::tagList(
        shiny::div(
          lapply(seq_len(min(effective_days(), 7)), function(i)
            shiny::tags$span(
              style = sprintf(
                "background:%s;color:#fff;border-radius:50%%;padding:2px 9px;
                 margin-right:5px;font-size:12px;font-weight:600;", DC[i]),
              paste("Day", i))),
          shiny::tags$span(style="color:#E91E8C;font-size:12px;margin:0 8px;","&#9632; Shopping"),
          shiny::tags$span(style="color:#5B9BD5;font-size:12px;","\u2014 Route  "),
          shiny::tags$span(style="color:#999;font-size:12px;","- - Cluster hull")),
        shiny::plotOutput("rplot", height="400px"),
        shiny::tags$p(style="color:#888;font-size:12px;margin-top:6px;",
                      "Arrows show travel direction \u00b7 distances on each leg \u00b7 pink squares = shopping"))
    }
    
    output$rplot <- shiny::renderPlot({
      it <- itin(); if (!length(it)) return(NULL)
      s  <- do.call(rbind, it)
      p  <- ggplot2::ggplot(s, ggplot2::aes(longitude, latitude)) + pthm() +
        ggplot2::labs(x="Longitude \u2192", y="Latitude \u2192")
      for (di in seq_along(it)) {
        d <- it[[di]]
        if (nrow(d) >= 3) {
          hi <- grDevices::chull(d$longitude, d$latitude)
          p  <- p + ggplot2::geom_polygon(
            data    = d[c(hi, hi[1]), ],
            ggplot2::aes(longitude, latitude),
            fill    = scales::alpha(DC[di], .09),
            colour  = DC[di], linetype="dashed", linewidth=.6)
        }
        if (nrow(d) >= 2)
          for (si in seq_len(nrow(d) - 1))
            p <- p +
              ggplot2::annotate("segment",
                                x=d$longitude[si], y=d$latitude[si],
                                xend=d$longitude[si+1], yend=d$latitude[si+1],
                                colour=DC[di], linewidth=.8,
                                arrow=ggplot2::arrow(length=ggplot2::unit(.15,"cm"), type="open")) +
              ggplot2::annotate("text",
                                x=(d$longitude[si]+d$longitude[si+1])/2,
                                y=(d$latitude[si]+d$latitude[si+1])/2,
                                label=paste0(d$leg_km[si+1],"km"),
                                size=2.8, colour="#555", vjust=-.4)
      }
      sh <- s[s$vibe == "Shopping", ]
      if (nrow(sh) > 0)
        p <- p + ggplot2::geom_point(data=sh,
                                     ggplot2::aes(longitude, latitude),
                                     shape=15, size=5, colour="#E91E8C")
      s$lbl <- abbreviate(s$attraction_name, minlength=2)
      p + ggplot2::geom_point(
        data   = s[s$vibe != "Shopping", ],
        ggplot2::aes(longitude, latitude, colour=factor(day)), size=6) +
        ggplot2::scale_colour_manual(
          values = setNames(DC[1:max(s$day)], as.character(1:max(s$day)))) +
        ggplot2::geom_text(data=s,
                           ggplot2::aes(longitude, latitude, label=lbl),
                           size=2.8, colour="white", fontface="bold")
    })
    
    # ── PCA Explorer ──────────────────────────────────────────
    p_ui <- function() {
      d <- cd()
      if (nrow(d) < 2) return(shiny::p("Not enough attractions for PCA. Try relaxing filters."))
      shiny::tagList(
        shiny::div(lapply(AV, function(v)
          shiny::tags$span(
            style = sprintf("color:%s;font-size:13px;margin-right:12px;", VC[v]),
            paste("\u25cf", v)))),
        shiny::plotOutput("pplot", height="400px"),
        shiny::tags$p(style="color:#888;font-size:12px;margin-top:6px;",
                      "PC1 separates free bazaars from ticketed monuments."))
    }
    
    output$pplot <- shiny::renderPlot({
      df <- pcd()
      if (!nrow(df) || !"PC1" %in% names(df)) return(NULL)
      df$lbl <- abbreviate(df$attraction_name, minlength=2)
      xr <- range(df$PC1, na.rm=TRUE); yr <- range(df$PC2, na.rm=TRUE)
      xp <- diff(xr) * .25; yp <- diff(yr) * .25
      ggplot2::ggplot(df, ggplot2::aes(PC1, PC2, colour=vibe)) + pthm() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill="#fff", colour="#E8E8E8"),
          panel.border     = ggplot2::element_rect(colour="#E8E8E8", fill=NA)) +
        ggplot2::labs(x="PC1 \u2014 budget vs premium \u2192",
                      y="PC2 \u2014 nature vs market \u2192") +
        ggplot2::geom_point(size=6, alpha=.85) +
        ggplot2::geom_text(ggplot2::aes(label=lbl),
                           colour="white", size=2.8, fontface="bold") +
        ggplot2::scale_colour_manual(values=VC) +
        ggplot2::coord_cartesian(
          xlim=c(xr[1]-xp, xr[2]+xp),
          ylim=c(yr[1]-yp, yr[2]+yp))
    })
    
    # ── For You (kNN) ─────────────────────────────────────────
    f_ui <- function() shiny::tagList(
      shiny::div(style="font-size:14px;font-weight:600;margin-bottom:12px;",
                 paste("kNN matches \u2014", paste(selv(), collapse=" + "))),
      shiny::uiOutput("kcards"))
    
    output$kcards <- shiny::renderUI({
      r <- recs()
      if (!nrow(r)) return(shiny::p("No recommendations found. Try selecting more vibes."))
      shiny::div(style="display:flex;flex-wrap:wrap;gap:10px;",
                 shiny::tagList(lapply(seq_len(min(6, nrow(r))), function(i) {
                   row <- r[i, ]; vc <- VC[row$vibe]; if (is.na(vc)) vc <- "#999"
                   fee <- if (row$entry_fee_inr == 0) "Free" else paste0("Rs", row$entry_fee_inr)
                   shiny::div(
                     style = "background:#fff;border-radius:10px;padding:14px 16px;
              box-shadow:0 1px 4px rgba(0,0,0,.07);
              width:calc(50% - 16px);display:inline-block;vertical-align:top;",
                     shiny::div(style="display:flex;justify-content:space-between;",
                                shiny::div(style="font-size:14px;font-weight:600;", row$attraction_name),
                                shiny::div(style=sprintf("color:%s;font-weight:700;font-size:14px;", vc),
                                           paste0(row$sim, "%"))),
                     shiny::div(style="font-size:12px;color:#888;margin-top:3px;",
                                sprintf("%s \u00b7 %s \u00b7 %s hrs \u00b7 %s \u2605",
                                        row$vibe, fee, row$avg_duration_hrs, row$rating)),
                     shiny::div(style="font-size:10px;color:#bbb;margin:6px 0 3px;","kNN similarity"),
                     shiny::div(style="background:#F0F0F0;border-radius:4px;height:5px;",
                                shiny::div(style=sprintf(
                                  "background:%s;width:%s;height:5px;border-radius:4px;",
                                  vc, paste0(max(10, row$sim), "%"))))
                   )
                 }))
      )
    })
    
    # ── Budget ────────────────────────────────────────────────
    b_ui <- function() {
      bv <- bud()
      if (is.null(bv$detail) || !is.data.frame(bv$detail) || nrow(bv$detail) == 0)
        return(shiny::p("No stops selected. Try relaxing filters or increasing Max Entry Fee."))
      
      bd <- bv$detail; mx <- max(bd$stop_spend, 1)
      shiny::tagList(
        shiny::div(
          pill(paste0("Rs", bv$entry),         "Entry fees",     "#6B70C4"),
          pill(paste0("Rs", bv$food),          "Food",           "#F0B429"),
          pill(paste0("Rs", bv$shop, " est."), "Shopping est.",  "#E91E8C"),
          pill(paste0("Rs", bv$total),         "Total est.",     "#4CAF82")),
        shiny::tags$br(), shiny::tags$br(),
        shiny::div(style="font-weight:600;font-size:14px;margin-bottom:10px;",
                   "Spend per stop"),
        shiny::div(
          style = "background:#fff;border-radius:12px;padding:16px;
            box-shadow:0 1px 4px rgba(0,0,0,.07);",
          shiny::tagList(lapply(seq_len(nrow(bd)), function(i) {
            row <- bd[i, ]; vc <- VC[row$vibe]; if (is.na(vc)) vc <- "#999"
            bw  <- paste0(pmax(4, round(row$stop_spend / mx * 60)), "%")
            fee <- if (row$entry_fee_inr == 0) {
              if (row$vibe == "Shopping") paste0("Rs", row$stop_spend, " est.")
              else if (row$vibe == "Food") paste0("Rs", row$stop_spend)
              else "Free"
            } else paste0("Rs", row$entry_fee_inr)
            shiny::div(style="display:flex;align-items:center;margin:8px 0;",
                       shiny::div(style="width:150px;font-size:13px;color:#333;flex-shrink:0;",
                                  row$attraction_name),
                       shiny::div(style=sprintf(
                         "background:%s;height:8px;border-radius:4px;width:%s;margin:0 14px;",
                         vc, bw)),
                       shiny::div(style="font-size:12px;color:#666;", fee))
          }))
        )
      )
    }
    
  } # end server
  
  shiny::shinyApp(ui = ui, server = server)
  
} # end tc_run