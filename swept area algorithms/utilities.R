# utilities.R

calculate_doorspread <- function(data) {
  # assume only one country
  country <- data$Country[1]
  cat(country, "\n")

  # calculating missing DoorSpread and Wingspread
  if (country == "FR") {

    data <- data %>%
      mutate(
        call_door = ifelse(
          !is.na(DoorSpread),
          DoorSpread, 47.548 + 0.296 * Depth
        ),
        call_wing = ifelse(
          !is.na(WingSpread),
          WingSpread,
          ifelse(
            !is.na(DoorSpread),
            0.1307 * 29.806 * Depth^0.1813 + 9.4306,
            0.1307 * DoorSpread + 9.4306
          )
        )
      )

  } else if (country == "GB-SCT") {
    data <- data %>%
      mutate(
        call_door = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          ifelse(
            !is.na(WingSpread),
            4.3277 * WingSpread - 3.784,
            24.481 * log(Warplngt) - 60.895
          )
        ),
        call_wing = ifelse(
          !is.na(WingSpread),
          WingSpread,
          ifelse(
            !is.na(DoorSpread),
            0.1909 * DoorSpread + 4.011,
            4.6235 * log(Warplngt) - 7.3296
          )
        )
      )
  } else if (country == "DK") {
    data <- data %>%
      mutate(
        call_door = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          ifelse(
            SweepLngt <= 60,
            79.386 - 33.695 * exp(-0.028 * Depth),
            104.502 - 316.682 * exp(-0.043 * Depth)
          )
        ),
        call_wing = ifelse(
          !is.na(WingSpread),
          WingSpread,
          ifelse(
            SweepLngt <= 60,
            5.867 + 0.206 * (79.386 - 33.695 * exp(-0.028 * Depth)),
            4.9 + 0.166 * (104.502 - 316.682 * exp(-0.043 * Depth))
          )
        )
      )
  } else if (country == "NL") {
    data <- data %>%
      mutate(
        call_door = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          case_when(
            Year <= 2004 ~ (29.544 * log10(Depth) + 14.116 * log10(Warplngt) + -3.456),
            Year %in% 2005:2014 ~ (31.165 * log10(Depth) + 0.2974 * log10(Warplngt) + 29.321),
            Year %in% 2015:2016 ~ (31.165 * log10(Depth) + 0.2974 * log10(Warplngt) + 29.321),
            Year > 2016 ~ (15.842 * log10(Depth) + 30.868 * log10(Warplngt) + -24.793)
          )
        ),
        call_wing = ifelse(
          !is.na(WingSpread),
          WingSpread,
          0.1909 * call_door + 4.011
        )
      )
  } else if (country == "SE") {
    data <- data %>%
      mutate(
        call_door = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          ifelse(
            SweepLngt <= 60,
            13.706 * log(Depth) + 26.853,
            29.489 * log(Warplngt) - 67.157
          )
        ),
        call_wing = ifelse(
          !is.na(WingSpread),
          WingSpread,
          ifelse(
            SweepLngt <= 60,
            15.78 * log(13.706 * log(Depth) + 26.853) - 48.284,
            21.231 * log(29.489 * log(Warplngt) - 67.157) - 77.605
          )
        )
      )

  } else if (country == "GB") {
    data <- data %>%
      mutate(
        call_door = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          case_when(
            Year <= 2005 & is.na(Warplngt) & is.na(WingSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2005 & !is.na(Warplngt) & is.na(WingSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2005 & is.na(Warplngt) & !is.na(WingSpread) ~ (4.616 * WingSpread - 15.966),
            Year = 2006 & is.na(Warplngt) & is.na(WingSpread) ~ (12.4680 * log(Depth) + 17.5865),
            Year = 2006 & !is.na(Warplngt) & is.na(WingSpread) ~ (16.4421 * log(Warplngt) - 24.4727),
            Year = 2006 & is.na(Warplngt) & !is.na(WingSpread) ~ (3.8182 * WingSpread - 11.9066),
            Year <= 2007 & is.na(Warplngt) & is.na(WingSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2007 & !is.na(Warplngt) & is.na(WingSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2007 & is.na(Warplngt) & !is.na(WingSpread) ~ (4.616 * WingSpread - 15.966)
          )
        ),
        call_wing = ifelse(
          !is.na(WingSpread),
          WingSpread,
          case_when(
            Year <= 2005 & is.na(Warplngt) & is.na(DoorSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2005 & !is.na(Warplngt) & is.na(DoorSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2005 & is.na(Warplngt) & !is.na(DoorSpread) ~ (4.616 * DoorSpread - 15.966),
            Year = 2006 & is.na(Warplngt) & is.na(WingSpread) ~ (3.1495 * log(Depth) + 8.2192),
            Year = 2006 & !is.na(Warplngt) & is.na(WingSpread) ~ (4.1885 * log(Warplngt) - 2.8637),
            Year = 2006 & is.na(Warplngt) & !is.na(WingSpread) ~ (0.2242 * DoorSpread + 5.7889),
            Year <= 2007 & is.na(Warplngt) & is.na(WingSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2007 & !is.na(Warplngt) & is.na(WingSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2007 & is.na(Warplngt) & !is.na(WingSpread) ~ (4.616 * DoorSpread - 15.966)
          )
        )
      )

    } else if (country == "DE") {
    data <- data %>%
      mutate(
        call_door = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          ifelse(
            SweepLngt <= 50,
            -7.456 + 3.616 * WingSpread + 3.124 * log(Depth),
            -0.441 + 10.009 * log(Warplngt) + 4.768 * log(Depth)
          )
        ),
        call_wing = ifelse(
          !is.na(WingSpread),
          WingSpread,
          case_when(
            !is.na(DoorSpread) & SweepLngt <= 50 ~ 3.359 + 0.095 * DoorSpread + 1.391 * log(Warplngt) + 0.261 * log(Depth),
            !is.na(DoorSpread) & SweepLngt > 50 ~ 3.087 + 0.118 * DoorSpread + 0.445 * log(Warplngt) + 0.368 * log(Depth),
            is.na(DoorSpread) & SweepLngt <= 50 ~ 3.317 + 2.341 * log(Warplngt) + 0.713 * log(Depth),
            is.na(DoorSpread) & SweepLngt > 50 ~ 3.087 + 0.118 * call_door + 0.445 * log(Warplngt) + 0.368 * log(Depth)
          )
        )
      ) %>%
      mutate(
        call_door =
          ifelse(
            is.na(DoorSpread) & is.na(WingSpread) & SweepLngt <= 50,
            -7.935 + (5.123 * call_wing) + 2.366 * log(Depth),
            call_door
          )
      )
  } else {
    return(NULL)
  }

  data %>%
    mutate(
      SweptAreaDSKM2 = call_distance * call_door / 1000000,
      SweptAreaWSKM2 = call_distance * call_wing / 1000000
    )
  # %>%
    #select(
    #  Country,
    #  call_door,
    #  call_wing,
    #  SweptAreaDSKM2,
    #  SweptAreaWSKM2
    #)
}
