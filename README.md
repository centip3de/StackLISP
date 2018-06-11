StackLISP
----
StackLISP is a stack based codegolfing language where code is data, much like in a LISP. This means that you can push down blocks of code onto the stack, manipulate them, use all operators on them, and then pop them and execute them when you want to. 

### Examples
Examples can be found in the `/examples` directory. Currently they're simply small tests of the language, however more complex ones will be added as the language progresses. 


As a brief example, here's fizzbuzz in `StackLISP` (in 40 bytes):
```
15[dd3s%0=s5s%0=[p[p]["Fizz"]i]["Buzz"]i.]f
```


### TODO:
* More OP codes
* Documentation
* Clean up the main eval function
* Add proper CLI arg parser
* Better error handling
* Figure out how to implement subtraction for blocks, now that they're implemented as free monads
