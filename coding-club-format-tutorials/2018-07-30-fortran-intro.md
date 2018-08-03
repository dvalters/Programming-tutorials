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

<a name="history"></a>

<a name="feeling"></a>

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


