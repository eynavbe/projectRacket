# projectRacket
To build a language to handle groups, we will call it SOL. A group as a sorted series of numbers without repetitions. In the language 3 primitive operations 1) Union 2) Cutting 3) Multiplication by scalar. In addition, similar to the FLANG language, there will be identifier names (with expressions) defining functions (fun expressions - functions that always receive two parameters).
<br/>
Types of function calls - in the language there are two types of function calls:
1) According to static-scoping expressions call-static- the function ran in an environment that extends the closure environment
 2) According to dynamic-scoping call-dynamic expressions - in such a call the function runs in an environment that expands the environment in which the call is made.
<br/>
In addition code in the SOL language for creating a pair object. The implementation will enter a global environment that will be initialized with each run of the interpreter that you build.
