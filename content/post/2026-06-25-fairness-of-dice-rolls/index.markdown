---
title: "Where's the Bias: In the Die, or in Me?"
author: Greg Foletta
date: '2026-09-30'
categories: [R Stan Bayesian]
---

It’s the classic mistake: trying to run before you can walk. And despite knowing my limitations when it comes to Bayesian modelling, I’ve still been making that mistake, trying to tackle problems that are outside of my capability. I needed something relatively simple and bounded, and while playing Yahtzee with my son I thought of a question that fit these parameters: how would I go about modelling the rolls of a die?

At first I thought this would be almost too simple, but as we’ll discover there’s complexity hidden in these simple questions. In this post we’ll start with a simple model the die’s rolls. But in doing so we’ll identify a couple of human biases that lead us to the wrong conclusions, and work on a different model of a die roll that can help to overcome these biases.

# LLM Disclosure

All commentary and code in this post was written by myself. An LLM was used to help me generate the Stan models for each of the scenarios, so please consider this post not as an expert lecturing to you ia innate knowledge, but rather as a student trying to understand and learn about these models by explaining how they work to someone else.

As a result, a significant amount of LLM time was used asking questions, clarifying, and trying to educate myself on the model and other supporting statistical aspects. This is my preferred method of interacting with a model: treating it as a tutor, not as a servant.[^1].

# Rolling as Meditation

<figure>
<img src="die.jpg" style="width:50.0%" alt="The Die in Question" />
<figcaption aria-hidden="true">The Die in Question</figcaption>
</figure>

The first step was to generate some data, which I acquired by rolling a die one-thousand times[^2]. This didn’t take as long as I thought it would - only about 30 minutes - it was actually quite a meditative process. Here’s a table showing the first 10 rolls:

``` r
die_rolls <-
    read_file('die_rolls.txt') |>
    str_split(pattern = '', simplify = FALSE) |>
    pluck(1) |>
    as.integer() |>
    as_tibble() |>
    rename(roll = value) |> 
    mutate(roll_id = 1:n()) |>
    select(roll_id, roll) |> 
    # Remove the newline (last value) which converted to NA
    slice_head(n = -1)
```

<div id="warhbmmgky" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#warhbmmgky table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#warhbmmgky thead, #warhbmmgky tbody, #warhbmmgky tfoot, #warhbmmgky tr, #warhbmmgky td, #warhbmmgky th {
  border-style: none;
}
&#10;#warhbmmgky p {
  margin: 0;
  padding: 0;
}
&#10;#warhbmmgky .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#warhbmmgky .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#warhbmmgky .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#warhbmmgky .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#warhbmmgky .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#warhbmmgky .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#warhbmmgky .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#warhbmmgky .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#warhbmmgky .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#warhbmmgky .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#warhbmmgky .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#warhbmmgky .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#warhbmmgky .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#warhbmmgky .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#warhbmmgky .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#warhbmmgky .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#warhbmmgky .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#warhbmmgky .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#warhbmmgky .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#warhbmmgky .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#warhbmmgky .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#warhbmmgky .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#warhbmmgky .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#warhbmmgky .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#warhbmmgky .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#warhbmmgky .gt_left {
  text-align: left;
}
&#10;#warhbmmgky .gt_center {
  text-align: center;
}
&#10;#warhbmmgky .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#warhbmmgky .gt_font_normal {
  font-weight: normal;
}
&#10;#warhbmmgky .gt_font_bold {
  font-weight: bold;
}
&#10;#warhbmmgky .gt_font_italic {
  font-style: italic;
}
&#10;#warhbmmgky .gt_super {
  font-size: 65%;
}
&#10;#warhbmmgky .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#warhbmmgky .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#warhbmmgky .gt_indent_1 {
  text-indent: 5px;
}
&#10;#warhbmmgky .gt_indent_2 {
  text-indent: 10px;
}
&#10;#warhbmmgky .gt_indent_3 {
  text-indent: 15px;
}
&#10;#warhbmmgky .gt_indent_4 {
  text-indent: 20px;
}
&#10;#warhbmmgky .gt_indent_5 {
  text-indent: 25px;
}
&#10;#warhbmmgky .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#warhbmmgky div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Die Rolls - First 10 of 1,000</td>
    </tr>
    &#10;  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">roll_id</th>
<td headers="stub_1_1 1" class="gt_row gt_right">1</td>
<td headers="stub_1_1 2" class="gt_row gt_right">2</td>
<td headers="stub_1_1 3" class="gt_row gt_right">3</td>
<td headers="stub_1_1 4" class="gt_row gt_right">4</td>
<td headers="stub_1_1 5" class="gt_row gt_right">5</td>
<td headers="stub_1_1 6" class="gt_row gt_right">6</td>
<td headers="stub_1_1 7" class="gt_row gt_right">7</td>
<td headers="stub_1_1 8" class="gt_row gt_right">8</td>
<td headers="stub_1_1 9" class="gt_row gt_right">9</td>
<td headers="stub_1_1 10" class="gt_row gt_right">10</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">roll</th>
<td headers="stub_1_2 1" class="gt_row gt_right">1</td>
<td headers="stub_1_2 2" class="gt_row gt_right">3</td>
<td headers="stub_1_2 3" class="gt_row gt_right">4</td>
<td headers="stub_1_2 4" class="gt_row gt_right">3</td>
<td headers="stub_1_2 5" class="gt_row gt_right">5</td>
<td headers="stub_1_2 6" class="gt_row gt_right">5</td>
<td headers="stub_1_2 7" class="gt_row gt_right">5</td>
<td headers="stub_1_2 8" class="gt_row gt_right">5</td>
<td headers="stub_1_2 9" class="gt_row gt_right">5</td>
<td headers="stub_1_2 10" class="gt_row gt_right">6</td></tr>
  </tbody>
  &#10;</table>
</div>

I show this primarily because of the ominous run of five fives (a probability of 1:7776), which had me concerned about my rolling technique. I then began swapping between my right and left hands every 100 rolls, and ensuring I put a fair amount of momentum into rolling the die.[^3]

After rolling, here’s the distribution of the number of rolls per face of the die:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" alt="" width="672" />
With apologies to Alexander Pope, “to pattern match is to be human”. What I’m drawn to is face six, and the fact that it’s peeking its head above all the others. Could this be an indication that the die has a bias in it? A spoiler alert: it’s not, and keen observers who understand the physical properties of a die will quickly see why it’s not. But there’s something about face six that still tickles something in my brain and leaves me wanting to understand more.

# A Trip to Monte Carlo

My original question was simply about modelling the die rolls, but now I’d seen the data I also wanted to determine the uncertainty of

``` stan
data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
  real alpha;
}

parameters {
  simplex[6] theta;
}

model {
  theta ~ dirichlet( rep_vector(alpha, 6) );
  roll  ~ categorical(theta); 
}
```

The Stan model takes the die roll data as an array of **rolls** of length **n**. We draw these rolls from a **categorical** distribution with with a simplex (a non-negative vector that sums to 1) parameters **theta**. The posterior distribution of **theta** we get by running our Stan sampler is the probability distribution over the six die face probabilities. This represents our uncertainty about the probability given our rolls data.

We’re using a non-informative **dirichlet** prior on theta, with all of the *alpha* values equal to one. By choosing this prior I’m saying that prior to seeing any data, I expect that all combinations of face probabilities have an equal probability density. So rolling one-thousand sixes and no other faces is equally as probabile as having an even spread. I’m holding the die in my hand, and I know that’s not reasonable, but I’m going to leave it as I think it’s instructive.

After compiling the Stan program, we feed it the data and sample from the posterior distributions of each of the **theta** parameters. Here’s a dotplot visualisation of these posterior draws, with a 90% credible interval and a vertical line at 1/6 (the probability of a side of a fair die).

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" alt="" width="672" />
Our posterior distribution for `\(\theta_6\)` is shifted to the right as compared to what we consider a fair die’s `\(theta\)` to be, which is `\(1/6\)`. The above graph shows a 90% credible interval, and again almost all of that is above the `\(1/6\)` as well.

How much of the mass is above 1/6?

``` r
posterior_mass <-
    die_roll_draws |>
    filter(face == 6) |>
    summarise(mean(theta > 1/6))

posterior_mass
```

    ## # A tibble: 1 × 2
    ##    face `mean(theta > 1/6)`
    ##   <int>               <dbl>
    ## 1     6               0.944

Around 95. Surely that means that there is a 95% probability that the real value of `\(\theta_6\)` is greater that 1/6, and thus the die is biased?

Well, not quite. I’ve made two mistakes here which end up being really valuable lessons, which ultimately lead to better insights and understanding. The first problem is that I’ve been caught out by the *Texan Sharpshooter Fallacy*. The second is that in an effort to be ‘objective’, I’ve chosen a really poor prior. Let’s dive a bit deeper into these.

# Enter the Texan Sharpshooter

This is why I - a statistical dabbler - am very nervous asserting anything about probability in public. Around every corner seems to be a wrong assumption or human foible that catches me out. In this instance, I’ve been shot by a [Texan Sharpshooter](https://en.wikipedia.org/wiki/Texas_sharpshooter_fallacy). Just like the sharpshooter drawing the target around the bullet holes after they’ve shot, I’ve chosen the die face with the largest count and have immediately seen bias where there is simply random variability.

To get a better intuition about this fallacy we can perform a simulation. In the code below we use `rmultinom()` to generate 100,000 simulations of 1,000 dice rolls. These come out in matrix format, so we do a bit of wrangling to turn it into a tibble:

``` r
simulations <- 100000
rolls <- 1000

die_roll_sims <-
    rmultinom(simulations, rolls, rep(1/6, 6)) |>
    as_tibble(.name_repair = 'unique_quiet') |> 
    mutate(face = 1:n()) |>
    pivot_longer(cols = starts_with('..'), names_to = 'sim', names_pattern = '..(\\d+)', values_to = 'count')
```

This data is slightly different to my original, manually rolled data; for each simulation we get a count of how many times each face was rolled. Now for each of those 100,000 simulations we, take consider two scenarios

1.  Imitate the fallacy: pick the face that had the maximum number of rolls out of the thousand.
2.  Chose a face beforehand (I’ve selected face 6).

``` r
die_roll_sim_summary_choices <-
    die_roll_sims |> 
    group_by(sim) |> 
    summarise(
        # Chosing the die with the max rolls (i.e. our face 6)
        chosen_after = max(count),
        # Chosing the face before we've seen the data
        chosen_before = count[face == 6]
    ) |>
    pivot_longer(cols = c(chosen_before, chosen_after), values_to = 'count')
```

Now we can visualise the distribution of counts for each of the scenarios, overlaying the counts for each of the die faces from or original data:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" alt="" width="672" />
Now we get a better view of differences in distributions of count. Looking at the count of face six from our original data, we can see it sits near the centre of the distribution of counts when chose the maximum face after we’ve rolled. It’s not that remarkable or surprising that our face six had as many rolls as it did.

But if we look at where the counts sit in the distribution when the face was chosen beforehand, we see it sits quite fair right in the distribution. It would have been remarkable and indicative that the die was biased.

# The Burden of Choosing a Prior

The second problem is with the model itself, and I briefly mentioned it before. I chose a `\(Dirichlet(1,1,1,1,1)\)` as the prior, which means that it is my belief that all combinations of `\(\theta_{1..6}\)` are equally likely.[^4]. This is not a reasonable prior as I mentioned before: I could see and feel the die and it looked fair. It certainly wasn’t going to roll all sixes.

What this relates to is my reluctance, or even fear, of choosing a prior. There’s a feeling that introducing a choice that I made somehow makes the model “less objective”, so I end up going for a very uninformative prior. As we’ve seen above this can have the effect of making the model worse, not better.

If you’re going use a more informative prior, there’s the burden of choosing which one? Part of the problem is that in these blog posts I’m not performing proper diagnostic checks of the model, one of which is prior predictive simulations. This would help narrow down what a reasonable prior would look like.

But even then, there’s still a wide range of values we could use as a prior. How can we get better at this?

# Refining the Model

The answer is to use a hierarchical model, and have the data help inform the prior. Here’s the new model we’ll try out on our die roll data:

``` stan
data {
  int<lower=1> n;
  array[n] int<lower=1, upper=6> roll;
}
parameters {
  real<lower=0> sigma;
  vector[6] eta_raw;
}
transformed parameters {
  vector[6] eta = sigma * eta_raw;
  simplex[6] theta = softmax(eta);
}
model {
  sigma   ~ normal(0, 0.5);
  eta_raw ~ std_normal();
  roll    ~ categorical(theta);
}
```

The model still takes the same data `\(roll\)` of length `\(n\)`, but our parameters have slightly changed. Our `\(theta\)` probabilities in our Categorical are not a direct parameter, but rather a transformed parameter. It’s derived from `\(eta\)`, which lives on the real number line and is transformed into probabilities via softmax().

Up until here there aren’t any substantial differences between the this and the first model, just a different path to get there. The difference in this model is that it’s hierarchical: we don’t fox the prior on `\(eta\)`, but rather assume `\(eta\)` is drawn from a `\(Normal(0, sigma)\)`, where `\(sigma\)` - what we could call the ‘lopsidedness’ of the die - is inferred from the data. There’s a slight detour via `\(eta_raw\)` to avoid [Neal’s Funnel](https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html), but `\(eta = sigma * eta_raw\)` is equivalent to a prior on `\(eta\)` of `\(Normal(0, sigma)\)`.

So what we now is a model that can infer the overall lopsidedness of the die (using `\(sigma\)`) and then adaptively regularise `\(theta\)` based on this. We can’t avoid a prior completely, needing one on `\(sigma\)`, but this prior is now informing the belief of the overall bias of the die, not each specific face of the die. The units of `\(sigma\)` are in log-ratios, which is difficult to comprehend, but effectively our prior of `\(HalfNormal(0, 0.5)\)` ultimately expects the die to have a substantial bias.

We now run the model with the original data:

Let’s take a look at the posterior distributions for the `\(theta\)` parameter:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" alt="" width="672" />
The posterior distributions for the die faces have been regularised back towards the fair value of 1/6. Why? Let’s look at the posterior distribution of `\(sigma\)`:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" alt="" width="672" />
The model has determined that when looking at the joint distribution of our pamaters that this is the posterior distribution of `\(sigma\)`. Most of the mass is concentrated near zero, meaning the model believes that the per-face `\(eta\)` scores do not vary far from zero. This indicates that the die is likely fair.

To get a better look at the contrast, let’s render only face six’s posterior distributions from our first model, and the hierarchical model:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" alt="" width="672" />
This more clearly shows the work the hierarchical model is doing to regularise face six back towards a fair die.

# Testing a Loaded Die

The obvious next step is to see how this hierachical model performs when we give it data from a die that is actually biased I don’t have a weighted die at hand, so I turn to a simulation. A loaded die would shift the centre of mass within the die towards one of the faces, incerasing its probabality, but then decreasing the probability of the opposing face. In the simulation below I’ve shifted the probability of rolling a six up by 0.05 and then reduced face one by the same amount.

``` r
set.seed(3455357)
shift = 0.050
probs <- c(1/6 - shift, 1/6, 1/6, 1/6, 1/6, 1/6 + shift)

loaded_die_rolls <- 
    tibble(
        roll_id = 1:1000,
        roll = sample(1:6, 1000, replace = TRUE, prob = probs)
    )
```

Here’s the distribution of dice rolls for the simulated loaded die:

``` r
loaded_die_rolls |>
    ggplot() +
    geom_bar(aes(roll), fill = 'lightgreen') +
    labs(
        title = "Biased Die Simulation - Distribution of One-Thousand Die Rolls",
        subtitle = "Faces 6 & 1 +/- (1/6) - 0.050",
        x = "Die Face",
        y = "Count of Rolls"
    ) +
    scale_x_continuous(breaks = 1:6)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" alt="" width="672" />
Let’s run this data through the same hierarchical model:

Here are the posterior distributions of each of the faces of the loaded die:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" alt="" width="672" />
The posteriors are not regularised at all, with faces one and six’s distributions sitting out to the left and right respectively. You’ll also notice that the 90% credible intervals does not overlap a fair value of 1/6. Remember: this is the same model, only the data has changed. Stil the model has been able to help us avoid the Sharpshooter Problem by pulling posterior likelihoods of a fair die back towards fair thetas, and conversely leaving in place posterior likelyhoods of a known biased die.

For clarity, here’s a view of theta\[6\] and sigma for both my manual die rolls, and the simulated biased die:

``` r
die_roll_draws_unified <-
bind_rows(
    loaded_die_roll_hierarchical_draws,
    die_roll_hierarchical_draws,
    .id = 'die_type'
) |>
    ungroup() |> 
    mutate(
        die_type = die_type |> recode_values( 
            "1" ~ "Biased Sim.",
            "2" ~ "Real"
        )
    )
```

``` r
die_roll_draws_unified |>
    filter(face == 6) |>
    rename(`theta[6]` = theta) |> 
    pivot_longer(c(sigma, `theta[6]`), names_to = 'parameter') |> 
    ggplot() +
    stat_dotsinterval(aes(x = value, y = die_type), .width = .9) +
    facet_grid(~parameter, scales = 'free') +
    theme(axis.text.y = element_text(angle = 90, hjust = .5)) +
    labs(
        title = "Posterior Distribution Comparison - Fair vs Loaded Die Rolls",
        x = "Parameter Posterior Density",
        y = "Die Type"
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-20-1.png" alt="" width="672" />
Note the `\(sigma\)` distribution for the biased die, with a 90% interval roughly sitting between .1 and .4, as opposed to the real, likely unbiased die where the mass is pushed closer to zero.

# Summary

That was an interesting journey, so time for a recap. We started off with the simple task of rolling a die one-thousand times and using a Stan model to model these rolls. We then saw how human intuition can lead us to the wrong results, and created a simulation to show this. Finally, we used a hierarchical model that took the data and determined the plausibility of a biased die, then adaptively regularising the per-face posterior distributions.

[^1]: I’ve made public the two key conversations [here](https://claude.ai/share/d7479b46-d067-4e5a-8452-cdda2825a177) and [here](https://claude.ai/share/803b18f1-7b5f-4458-8089-fbc50c2b82fd)

[^2]: You can view the raw data [here](die_rolls.txt)

[^3]: Whether or not these factors had an effect on the die is a complete post in and of itself.

[^4]: Dirchlet is conjugate to our Categorical, so each `\(\alpha\)` in the prior works as a pseudo-count added to our actual roll data
