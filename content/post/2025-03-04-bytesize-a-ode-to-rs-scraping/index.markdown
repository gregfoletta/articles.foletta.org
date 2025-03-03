---
title: 'Bytesize: A Ode to Rs Scraping'
author: ''
date: '2025-03-03'
categories: [R Scraping]
---




I was recently doing some websscraping, building out a data pipeline to generate the end-of-season awards for Little Athletics. I ran into a problem, and as always, R comes to the rescue with a simple, elegant solution. I thought I'd write a little byte-sized ode to web-scraping with R.

# Beauty and Terseness

I'll get to the challenge I ran into shortly, but before that let's take a look at what standard web-scraping scenario looks like, and how easy is is to do in R. Suppose you want to get all the headlines from The Age's website. You look at the source and see that they have an attribute *data-testid* equal to *article-link*. Here's the pipeline that acheives this:


``` r
request('http://theage.com.au') |>
    req_perform() |>
    resp_body_html() |>
    html_elements(xpath = "//a[@data-testid='article-link']") |>
    html_text() |>
    tibble(.name_repair = ~c('headline')) |>
    filter(headline != "") |>
    slice_head(n = 10)
```

```
# A tibble: 10 × 1
   headline                                                                     
   <chr>                                                                        
 1 The Morning Edition podcast                                                  
 2 Get 2-for-1 Comedy Festival tickets*                                         
 3 Thank God it’s Monday newsletter                                             
 4 This radical move will keep kids safer during the school run – but it’s unpo…
 5 The missing links in Melbourne’s traffic nightmare – and how to fix them     
 6 Coalition would end ‘unsustainable’ work from home for public servants       
 7 ‘It’s gonna cost me’: Home owner says he had no choice but to ‘illegally’ bu…
 8 It’s clear that Trump is an agent of Putin. All US allies should be alarmed  
 9 Plan to cram classrooms in schools to keep up with population demands        
10 The pay gap at Melbourne’s private girls’ schools                            
```

There we go, five lines of R and you've got the headlines, plus a couple more to get it into a nicer structure. Why is it so easy? I think it comes down to two things: number one is R's pipe operator which means you don't have to pepper your code with temporary variables. Second is R's vectorisation, which means you don't need to worry about any loops.

# The Challenge

The challenge I ran into yesterday was that, while the data I needed was structured, it wasn't in HTML, XML, or even JSON, it was actually JavaScript. Here's an sample of what was returned in an API call:

```json
sessions_NMRKeilor = [
  {"SessNbr":"1","SessPtr":"24","SessName":"Sat Morning - Field","SessDay":"1","SessTime":"30600"},
  {"SessNbr":"2","SessPtr":"25","SessName":"Sat Morning - Track","SessDay":"1","SessTime":"32400"},
  {"SessNbr":"3","SessPtr":"46","SessName":"Sat Afternoon - Field","SessDay":"1","SessTime":"46800"},
  ...
]
```

Not sure what to do, I go and fetch thed data:


``` r
js_content <-
    request('https://lavic.resultshub.com.au/php/resultsFileFetch.php?season=2024&series=regions&round=3&venue=undefined') |>
    req_perform() |>
    resp_body_string()
```

The mind initially goes to dark places: can I solve this with a regex? Pausing for a second, I think "this is just JavaScript, is there a way I can simply evaluate it?". A quick seach shows that there's an R library that provides API access into Google's V8 JavaScript implementation, allowing us to evalusate the code we received:



From there we can get the variable we need, and we're done.


``` r
jscontext$get('sessions_NMRKeilor') |>
    as_tibble() 
```

```
# A tibble: 8 × 5
  SessNbr SessPtr SessName              SessDay SessTime
  <chr>   <chr>   <chr>                 <chr>   <chr>   
1 1       24      Sat Morning - Field   1       30600   
2 2       25      Sat Morning - Track   1       32400   
3 3       46      Sat Afternoon - Field 1       46800   
4 4       30      Sat Afternoon - Track 1       46800   
5 5       43      Sun Morning - Field   2       30600   
6 6       38      Sun Morning - Track   2       30600   
7 7       47      Sun Afternoon - Field 2       46800   
8 8       45      Sun Afternoon - Track 2       47700   
```

