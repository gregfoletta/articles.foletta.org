---
title: Ergo Bike Wind Resistance
author: Greg Foletta
date: '2020-10-24'
slug: []
categories: []
tags: []
images: []
---



```r
# Read in the XML files
tcx <-
    tibble(
        files = list.files(
            path = here('static', 'post', 'ergo_bike'),
            pattern = '*.tcx',
            full.names = TRUE
        )
    ) %>%
    transmute(xml = map(files, ~read_xml(.x))) %>% 
    mutate(.id = 1:n())

print(tcx)
```

```
## # A tibble: 20 x 2
##    xml          .id
##    <list>     <int>
##  1 <xml_dcmn>     1
##  2 <xml_dcmn>     2
##  3 <xml_dcmn>     3
##  4 <xml_dcmn>     4
##  5 <xml_dcmn>     5
##  6 <xml_dcmn>     6
##  7 <xml_dcmn>     7
##  8 <xml_dcmn>     8
##  9 <xml_dcmn>     9
## 10 <xml_dcmn>    10
## 11 <xml_dcmn>    11
## 12 <xml_dcmn>    12
## 13 <xml_dcmn>    13
## 14 <xml_dcmn>    14
## 15 <xml_dcmn>    15
## 16 <xml_dcmn>    16
## 17 <xml_dcmn>    17
## 18 <xml_dcmn>    18
## 19 <xml_dcmn>    19
## 20 <xml_dcmn>    20
```

```r
pull_tcx_data <- function(tcx) {
    # Strip out the namespace and 
    # pull out each trackpoint
    trackpoints <- 
        tcx %>% 
        xml_ns_strip() %>% 
        xml_find_all('.//Trackpoint')
    
    # Timestamp for each trackpoint
    time <-
        trackpoints %>% 
        xml_find_all('./Time') %>% 
        xml_text() %>% 
        ymd_hms()
        
    # Cadence at each trackpoint
    cadence <-
        trackpoints %>%
        xml_find_all('./Cadence') %>% 
        xml_text() %>% 
        as.integer()
   
    # There is sometimes a leading timestamp with no data.
    # If so, we strip the first timestamp out
    if (length(time) == length(cadence) + 1) {
        time <- time[-1]
    }
    
    # Power at each trackpoint
    power <-
        trackpoints %>% 
        xml_find_all('./Extensions/TPX/Watts') %>% 
        xml_text() %>% 
        as.integer()
    
    # Create the data frame 
    tibble(datetime = time, cadence = cadence, power = power)
}
```


```r
ergo_data <-
    tcx %>% 
    mutate(tcx = map(xml, ~pull_tcx_data(.x))) %>% 
    unnest(cols = 'tcx') %>% 
    select(-xml)
print(ergo_data)
```

```
## # A tibble: 46,338 x 4
##      .id datetime            cadence power
##    <int> <dttm>                <int> <int>
##  1     1 2020-06-14 04:21:00      64   156
##  2     1 2020-06-14 04:21:01      64   162
##  3     1 2020-06-14 04:21:02      63   154
##  4     1 2020-06-14 04:21:03      65   178
##  5     1 2020-06-14 04:21:04      66   178
##  6     1 2020-06-14 04:21:05      67   175
##  7     1 2020-06-14 04:21:06      67   173
##  8     1 2020-06-14 04:21:07      66   181
##  9     1 2020-06-14 04:21:08      66   184
## 10     1 2020-06-14 04:21:09      66   183
## # … with 46,328 more rows
```

```r
ergo_data <-
    ergo_data %>% 
    group_by(.id) %>% 
    mutate(second = as.integer(datetime - first(datetime)))

ergo_data %>% 
    filter(.id <= 6) %>% 
    ggplot() +
    geom_line(aes(second, power)) +
    facet_wrap(~.id, scales = 'free') +
    labs(
        title = 'Power Over Time',
        x = 'Seconds',
        y = 'Watts'
    )
```

<img src="index_files/figure-html/unnamed-chunk-5-1.png" width="672" />


# Theory

Drag equation:

$$ F = \frac{1}{2} \rho v^2 C_D A $$
# Training the Model



