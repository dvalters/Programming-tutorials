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

We are going to start with a 'high-level' view of a very simple Fortran program. Don't worry about every piece of syntax and keyword at the minute - we're just going to look at the overall general structure.

This program calculates the area of a triangle using Heron's formula, i.e. : https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap03/heron.html

```fortran
! ------------------------------------------------------
! Compute the area of a triangle using Heron's formula
! ------------------------------------------------------

PROGRAM  HeronFormula
   IMPLICIT  NONE

   REAL     :: a, b, c             ! three sides
   REAL     :: s                   ! half of perimeter
   REAL     :: Area                ! triangle area
   LOGICAL  :: Cond_1, Cond_2      ! two logical conditions

   READ(*,*)  a, b, c

   WRITE(*,*)  "a = ", a
   WRITE(*,*)  "b = ", b
   WRITE(*,*)  "c = ", c
   WRITE(*,*)

   Cond_1 = (a > 0.) .AND. (b > 0.) .AND. (c > 0.0)
   Cond_2 = (a + b > c) .AND. (a + c > b) .AND. (b + c > a)
   IF (Cond_1 .AND. Cond_2) THEN
      s    = (a + b + c) / 2.0
      Area = SQRT(s * (s - a) * (s - b) * (s - c))
      WRITE(*,*) "Triangle area = ", Area
   ELSE
      WRITE(*,*) "ERROR: this is not a triangle!"
   END IF

END PROGRAM  HeronFormula
```

Lines 1-3:

The first three lines are comment lines - you will hopefully find a lot of these in Fortran programs you are given, and in the ones you write yourself. Comment lines are just notes or explanations that help the programmer and the user. They are never executed by the computer and you can write whatever you like within any line marked as a comment. In Fortran this is the exclamation mark (!). Any line beginning with an exclamtion mark will be ignored by the computer when the program runs. Comments help the user to understand more complicated bits of code by providing a more human-readable explanation, or perhaps giving an example of how to use the code.

Line 5: Begin the program! Fortran is quite a verbose language, in other words, we have to be quite explicit in telling it what we are about to do (Contrast with Python and R which are said to be more dynamic or intuitive languages and meaning can often be inferred.)

So here we are just telling fortran that we wish to begin our program, and we can optionally give it a name. Skip down to the last line: notice how we also have an "END PROGRAM" statement. Fortran likes to keep things balanced and know exactly when you have ended sections of code. You will see the END statement used often to demarcate sections of the code such as loops, functions, and so on.

The next section of the program (lines 6-17) is where we define the variables to be used in the program. In Fortran programs, you will almost always see these defined at the very top of the program, unlike in other languages where you can define them as you go along. Fortran likes to know at the start which variables it is dealing with. We will cover what the different types are in the next section. Note also how we have added comments to explain in more human-readable format what each variable does.

The remaining bit of the program is the actual bit where the area of the triangle is calculated. Our pre-declared variables have values assigned to them using common mathematical operators and fucntions. (*, /, +, -, SQRT, etc...) and we also write out the result to the screen using the WRITE function. We also terminate our program using the END PROGRAM statement.

This program would be saved like a plain-text file, but we would give it the extension `.f90`. (By convention). To run the program, we would first need to _compile_ it, which is covered in a later section of the tutorial.
 

<a name="basics"></a>

## Basics

Fortran has a set of rules used to determine whether a program is valid and can be understood by the computer, a bit like a human language. The combination of keywords and characters that are used to form Fortran programs are usually referred to as the language's _syntax_. Using the above triangle program as a starting point, we will cover the very basic syntax of the Fortran language. 

### Program structure

Fortran programs begin with the `PROGRAM` keyword, followed, optionally, by a name for the program. The end of the program should also be marked by `END PROGRAM`.

e.g.:

```fortran
PROGRAM MyProgram
  ! Do some stuff here
END PROGRAM MyProgram
```

Within the `PROGRAM` statements, your Fortran program can define functions, declare variables to be used in these functions, just like in other programming languages such as R or Python. Within these statements, this is where the calculations on data are performed in the program. 

### Defining variables

Variables represent data or values used in your program. A variable can represent a single value, like the expression `x = 3`, or a variable can refer to a larger structure for holding data, such as a table or a list of values. Variables can also be used to store non-numeric values such as text, letters, and words, for example. 

We refer to variables in Fortran as having certain _Types_. A type describes what kind of data the variable is expected to contain. For example, the expression `x = 3` we could say that x refers to a _number_, i.e. the number 3. If we had something like: `x = "cat"`, we might say that x refers to some _characters_ or _letters_. These are examples of how we could describe our variables using keywords to keep track of what kind of data we are using.

In Fortran, we are required to be specific about what kind of data our variables are. Fortran has a set of keywords that are used to define the different types of variables. For example to define some _INTEGER_ variables for use in a later calculation, we would write in Fortran:

```fortran
INTEGER :: n = 3
INTEGER :: m = 6
```

We are saying with these statements that we want to create two integers, `n` and `m` and that they will be assigned the values 3 and 6 respectively. (We can delay assigning values until later on in the program, if necessary.)

Fortran has different types for numeric values. For example, if our calculations required non-integer numbers, such as 3.141, we would use a type called _REAL_. I.e. a real number.

```fortran
REAL :: x = 1.0
REAL :: pi = 3.141
```

Once we have create variables in a Fortran program, we can manipulate them and change them.

Variables in fortran are usually defined in the first few lines of the program or a function, like in our example program above. There is one more type that is introduced in the example program, the `LOGICAL` type. This type refers to variables that are used as True or False values. 

```fortran
LOGICAL :: Cond_1, Cond_2
```

Note that we can can define multiple variables on the same line, separated by a comma. This saves space and typing, and would be the equivalent of writing:

```fortran
LOGICAL :: Cond_1
LOGICAL :: Cond_2
```

Variable names may be made up of standard latin-alphabet characters, underscores, and numbers. Note that Fortran is *not* case-sensitive. Keywords and variable names may be written in uppercase or lowercase. Fortran does not distinguish between uppercase and lowercase names, so be careful of using them in variable names. For example: `Cond_1` is the same as `cond_1`.

By convention, you will often see Fortran keywords written in `UPPERCASE`, though this is not a requirement. For example, REAL, INTEGER, IF, ELSE, PROGRAM, and so on. You can chose to use either uppercase or lowercase, but it is good practice to be consistent in their use, for readability.

### Displaying messages and values

When you are running a Fortran program, the easiest way to see the results of the calculation or other outputs are to print them to the terminal, command line, or console. (These are all terms used interchangeably to refer to the same thing - the window where commands can be entered and text is printed out to screen.)

You also need a way of inputting data to the program. This can also be done (for this simple example program) by typing the values directly into the terminal from the keyboard. (In a later tutorial we may cover reading information in from text files).

Fortran has two useful functions that will get you started in reading-in and displaying data and messages to the screen: the `READ` function and the `WRITE` function.

#### `READ`

The `READ` function tells the fortran program to record the values you enter via the keyboard and store them in variables that you define in your program. In our example triangle program we use `READ` like this:

```fortran
READ(*,*)  a, b, c
```

In Fortran functions, any inputs needed for the function are placed inside a list within the two round-brackets (parentheses), as in `READ(input1, input2, etc...)`. In `READ`, The first asterisk (*) means the input comes from the keyboard in a READ statement and goes to the screen in a WRITE statement. The second asterisk (*) means the computer decides how the I/O elements should look based on the TYPE of data in the list of variables that follow.

The list of variables that follow will be assigned the values that you type, one at a time. So in our program we have three variables (a, b, c) that we have already defined earlier on. When the program is running and gets to the READ statement, it will prompt you to enter a variable and press enter. This will happen three times, and each value you type will be assigned to `a`, `b` and `c` respectively.

The WRITE function is very similar, but it just prints out the variables to the screen, in the order specified. So:

```fortran
WRITE(*,*) a, b, c
```

Would print out the values assigned to a, b, and c.

### Mathematical operations

Fortran is designed primarily for numerical calculations, and it has many built-in functions for mathematical operations. In the example triangle-area program, we use the basic functions: `+, -, *, /` for addition, subtraction, multiplication, and division. There is one other that we use in the above example, `SQRT()`, which finds the square root of a number. 

### Logical expressions

Logical expressions are statements like "If A and B are TRUE, then do C", or "If value X is less than 10, then divide X by Y". They are ways of controlling the behaviour of a program based on comparisons or truth-values (LOGICAL types in Fortran.) 

The next part of the triangle-program, after the READ and WRITE statements, starts to calculate two conditions (tru/false values) for the last part of the calculation:

```fortran
Cond_1 = (a > 0.0) .AND. (b > 0.0) .AND. (c > 0.0)
Cond_2 = (a + b > c) .AND. (a + c > b) .AND. (b + c > a)
```

Recall that `Cond_1` and `Cond_2` are `LOGICAL` types, i.e. they can either be TRUE or FALSE. 

The true or false value assigned to these two variables will depend on the outcome of evaluating the expression on the right hand side of the equals sign.

Two logical operators are used in this example: the greater-than operator `>` and the `.AND.` operator. Greater than behaves in the same way as its mathematical counterpart. E.g. `1 > 2` would give the answer FALSE, because one is not greater than two. 

The `.AND.` operator checks to see if both expressions either side are true or false. E.g. `(1 < 2) .AND. (3 < 4)` would return TRUE because both expressions are TRUE. Notice that we can use brackets, similalrly in a mathematical context, to state the order in which we want expression to be evaluated. First the computer will evaluate the expression in the brackets, before proceding to evaluating the `.AND.` expression(s) as a whole.

The outcome of these two lines (TRUE/FALSE) will depend on the inputs you give the program when it runs, which we will come to soon.

### Making decisions in a program

All but the most trivial programs will have to make decisions depending on the type of data input or the results of a calculation. We can refer to this as the program flow or program logic. It is a bit like the logical decisions we make in everyday life:

"IF it is raining today, THEN I will take my umbrella with me, or ELSE I will take my sunhat."

The words in uppercase also happen to be the same keywords Fortran uses to control the logical flow of a program depending on different conditions.

Let's look at the final section of the triangle-area program:


```fortran
   Cond_1 = (a > 0.) .AND. (b > 0.) .AND. (c > 0.0)
   Cond_2 = (a + b > c) .AND. (a + c > b) .AND. (b + c > a)
   IF (Cond_1 .AND. Cond_2) THEN
      s    = (a + b + c) / 2.0
      Area = SQRT(s * (s - a) * (s - b) * (s - c))
      WRITE(*,*) "Triangle area = ", Area
   ELSE
      WRITE(*,*) "ERROR: this is not a triangle!"
   END IF
```



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


