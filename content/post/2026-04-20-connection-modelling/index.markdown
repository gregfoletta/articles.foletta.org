---
title: "Me and My LLM: Iteratively Fitting Bayesian Network Traffic Models"
author: Greg Foletta
date: '2026-04-20'
slug: []
categories: [R Networking LLM]
tags: []
---


    
I've worked in the network and security industry for almost two decades, and in that time I've spent a significant portion sizing and deploying routers and firewalls for customers. A router is relatively simple to size: what's their aggregate bandwidth they have or are forceasting they will have. With a firewall bandwidth also needs to be taken into account, but as they are stateful devices you also need to look at connections per second, and concurrent connections. This has flow on effects as well, for example the firewalls will often log their connections, and so your log storage rates and volumes will be based on the aggregate these connections across all the customer's locations.

Both mine and others methods for sizing firewalls has always irked me for being a too haphazard: either finger in the air numbers or using averages from customer data. Neither felt rigorous enough for me.

What I wanted to do was create a Bayesian model based on real world data. Using this model, connections per second for x number of users could be modeled taking into account both the uncertainty of the model, and the risk appetite of the customer. In this article I'll take you through the simple models I started to build and then how, when I ran into a brick wall, I used an LLM to iteratively work on the problem to come up with an answer.

I've tried to keep this post as short as possible while still communicating the core ideas. Still, if you'd like to jump straight to reading about the final model, you can clik [here](#the-final-llm-model).


# The Data and Caveats


# My Attempts

# The Iteratve LLM Environment

# The Final LLM Model

# The Start, Not the End
