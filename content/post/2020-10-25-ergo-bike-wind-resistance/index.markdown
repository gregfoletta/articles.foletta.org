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
    rownames_to_column(var = '.id')
print(tcx)
```

```
## # A tibble: 9 x 2
##   .id   xml       
##   <chr> <list>    
## 1 1     <xml_dcmn>
## 2 2     <xml_dcmn>
## 3 3     <xml_dcmn>
## 4 4     <xml_dcmn>
## 5 5     <xml_dcmn>
## 6 6     <xml_dcmn>
## 7 7     <xml_dcmn>
## 8 8     <xml_dcmn>
## 9 9     <xml_dcmn>
```

```r
pull_tcx_data <- function(tcx) {
    # Strip out the namespace
    trackpoints <- 
        tcx %>% 
        xml_ns_strip() %>% 
        xml_find_all('.//Trackpoint')
        time <-
        trackpoints %>% 
        xml_find_all('./Time') %>% 
        xml_text() %>% 
        ymd_hms()
       # There's one extra at the start we need to remove 
    time <- time[-1]
       # Pull out the cadence at each trackpoint 
    cadence <-
        trackpoints %>%
        xml_find_all('./Cadence') %>% 
        xml_text() %>% 
        as.integer()
        # Pull out the power at each trackpoint
    power <-
        trackpoints %>% 
        xml_find_all('./Extensions/TPX/Watts') %>% 
        xml_text() %>% 
        as.integer()
        # Create the data frame 
    tibble(
        datetime = time,
        cadence = cadence,
        power = power
    )
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
## # A tibble: 19,624 x 4
##    .id   datetime            cadence power
##    <chr> <dttm>                <int> <int>
##  1 1     2020-08-26 22:12:01      63   153
##  2 1     2020-08-26 22:12:02      64   172
##  3 1     2020-08-26 22:12:03      64   179
##  4 1     2020-08-26 22:12:04      65   171
##  5 1     2020-08-26 22:12:05      64   143
##  6 1     2020-08-26 22:12:06      64   181
##  7 1     2020-08-26 22:12:07      65   170
##  8 1     2020-08-26 22:12:08      65   164
##  9 1     2020-08-26 22:12:09      66   190
## 10 1     2020-08-26 22:12:10      67   190
## # … with 19,614 more rows
```

```r
ergo_data <-
    ergo_data %>% 
    group_by(.id) %>% 
    mutate(second = datetime - first(datetime))
```

```r
ergo_data %>% 
    ggplot() +
    geom_point(aes(cadence, power), alpha = .1)
```

<img src="index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

