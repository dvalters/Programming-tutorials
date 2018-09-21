---
layout: post
title: Introduction to Fortran
subtitle: The basics of the Fortran programming language
date: 2018-07-30 00:00:00
author: Declan Valters
meta: "FortranIntro"
tags: fortran
---

<div class="block">
  <center><img src="{{ site.baseurl }}/img/tutheader_fortran.png" alt="Img"></center>
</div>

### Tutorial aims:

#### <a href="#understanding"> 1. Understand what the Fortran progamming langauge is</a>

#### <a href="#history"> 2. Learn about a brief history of Fortran</a>

#### <a href="#feeling"> 3. Understand how Fortran differs to other programming languages</a>

#### <a href="basics"> 4. Learn some of the basic syntax of the Fortran language</a>

#### <a href="compiling" 5. Learn how to compile a basic Fortran program</a>

#### <a href="structure" 6. Learn how to compile, configure, and run a larger Fortran program</a>

<a name="understanding"></a>

## What is Fortran?

Fortran is a computer programming language that is extensively used in numerical, scientific computing. While outwith the scientific community, Fortran has declined in popularity over the years, it still has a strong user base with scientific programmers, and is also used in organisations such as weather forecasters, financial trading, and in engineering simulations. Fortran programs can be highly optimised to run on high performance computers, and in general the language is suited to producing code where performance is important. 
Fortran is a _compiled_ language, or more specifically it is compiled ahead-of-time. In other words, you must perform a special step called *compilation* of your written code before you are able to run it on a computer. This is where Fortran differs to *interpreted* languages such as Python and R which run through an interpreter which executes the instructions directly, but at the cost of compute speed.

<a name="history"></a>

## A brief Fortran history

Fortran was originally named after the contraction of *Formula Translation*, highlighting Fortran's origins as a language designed specifically for mathematical calculations. Fortran was developed in the early 1950s and the first ever Fortran program ran in 1954 - making Fortran fairly unusual among programming languages in that it predates the modern _transistor_ computer - the first Fortran program ran on the IBM 704 vacuum tube computer! Fortran has outlived several nation states since its conception, and still is in wide use today in a number of specialised scientific communities. Unfortunately Fortran is often referred to as an 'outdated' or 'legacy' programming language. I disagree with this description, as although Fortran has a long history, the language continues to be updated, new features are developed and added to the Fortran language standard, and there is still a strong community behind Fortran. The latest Fortran standard was released in 2018, bringing many new features and keeping fortran a relevnat, highly performant language for contemporary scientific computing challenges.

<a name="feeling"></a>

## Getting a feel for Fortran

Perhaps you have previously used other programming languages, such as Python, R, or MATLAB, which have developed with easy to understand syntax in mind, and with a programming style that favours more rapid development time at the expense of computational performance. Fortran will seem different to these languages in many ways, but the principles of programming remain broadly the same, and some syntax is shared or similar to elements of other programming languages.

<a name="basics"></a>

<a name="compiling"></a>

<a name="structure"></a>


# Summary

### Tutorial outcomes:

#### 1.

#### 2.

#### 3.


<hr>
<hr>

<h3><a href="https://www.surveymonkey.co.uk/r/WVL5GXB" target="_blank">&nbsp; We would love to hear your feedback, please fill out our survey!</a></h3>
<br>
<h3>&nbsp; You can contact us with any questions on <a href="mailto:ourcodingclub@gmail.com?Subject=Tutorial%20question" target = "_top">ourcodingclub@gmail.com</a></h3>
<br>
<h3>&nbsp; Related tutorials:</h3>
{% for post in site.posts %}
	{% if post.url != page.url %}
  		{% for tag in post.tags %}
    			{% if page.tags contains tag %}
<h4><a style="margin:0 padding:0" href="{{ post.url }}">&nbsp; - {{ post.title }}</a></h4>
  			{% endif %}
		{% endfor %}
	{% endif %}
{% endfor %}
<br>
<h3>&nbsp; Subscribe to our mailing list:</h3>
<div class="container">
	<div class="block">
        <!-- subscribe form start -->
		<div class="form-group">
			<form action="https://getsimpleform.com/messages?form_api_token=de1ba2f2f947822946fb6e835437ec78" method="post">
			<div class="form-group">
				<input type='text' class="form-control" name='Email' placeholder="Email" required/>
			</div>
			<div>
                        	<button class="btn btn-default" type='submit'>Subscribe</button>
                    	</div>
                	</form>
		</div>
	</div>
</div>

<ul class="social-icons">
	<li>
		<h3>
			<a href="https://twitter.com/our_codingclub" target="_blank">&nbsp;Follow our coding adventures on Twitter! <i class="fa fa-twitter"></i></a>
		</h3>
	</li>
</ul>


