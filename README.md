# lox implementations

Implementations of the lox language from [Crafting Interpreters](https://craftinginterpreters.com/)

`treewalk/` is the treewalk interpreter implementation, and
`bytecode/` is the bytecode interpreter implementation

Both are written in zig 0.11.0.

More information for each interpreter is in each directory.

## Example Output

This output is from the interpreter in `bytecode/`.

From 2a6c616

```
$ zig build run -- ./examples/breakfast.lox
--------------------------- ./examples/breakfast.lox ---------------------------
var food = "toast";
var beverage = "coffee";

var day = "saturday";
if (day == "saturday") {
  food = "waffles";
}

var breakfast = food + " with " + beverage;

print breakfast;

-------------------------------- constants table -------------------------------
00: food
01: toast
02: beverage
03: coffee
04: day
05: saturday
06: day
07: saturday
08: food
09: waffles
0a: breakfast
0b: food
0c:  with 
0d: beverage
0e: breakfast
------------------------------- compiled bytecode ------------------------------
0x0000: 01 01 10 00 01 03 10 02 01 05 10 04 11 06 01 07 
0x0010: 0b 18 00 09 0f 01 09 12 08 0f 17 00 01 0f 11 0b 
0x0020: 01 0c 03 11 0d 03 10 0a 11 0e 0e 07 00 
----------------------------------- <script> -----------------------------------
offset byte line meaning          encoded data    

               1 var food = "toast";
0x0000 0x01      constant                          |  
0x0001 0x01        (constant)     "toast"          |  
0x0002 0x10      define_global                     |  
0x0003 0x00        (constant)     "food"           |  

               2 var beverage = "coffee";
0x0004 0x01      constant                          |  
0x0005 0x03        (constant)     "coffee"         |  
0x0006 0x10      define_global                     |  
0x0007 0x02        (constant)     "beverage"       |  

               4 var day = "saturday";
0x0008 0x01      constant                          |  
0x0009 0x05        (constant)     "saturday"       |  
0x000a 0x10      define_global                     |  
0x000b 0x04        (constant)     "day"            |  

               5 if (day == "saturday") {
0x000c 0x11      get_global                        |  
0x000d 0x06        (constant)     "day"            |  
0x000e 0x01      constant                          |  
0x000f 0x07        (constant)     "saturday"       |  
0x0010 0x0b      equal                             |  
0x0011 0x18      jump_if_false    0x0011 -> 0x001d |  
0x0012 0x00        (jump hi byte)                  |  
0x0013 0x09        (jump lo byte)                  |  
0x0014 0x0f      pop                               |  

               6   food = "waffles";
0x0015 0x01      constant                          |  
0x0016 0x09        (constant)     "waffles"        |  
0x0017 0x12      set_global                        |  
0x0018 0x08        (constant)     "food"           |  
0x0019 0x0f      pop                               |  

               7 }
0x001a 0x17      jump             0x001a -> 0x001e |  
0x001b 0x00        (jump hi byte)                  |  
0x001c 0x01        (jump lo byte)                  |  
0x001d 0x0f      pop                               |  

               9 var breakfast = food + " with " + beverage;
0x001e 0x11      get_global                        |  
0x001f 0x0b        (constant)     "food"           |  
0x0020 0x01      constant                          |  
0x0021 0x0c        (constant)     " with "         |  
0x0022 0x03      add                               |  
0x0023 0x11      get_global                        |  
0x0024 0x0d        (constant)     "beverage"       |  
0x0025 0x03      add                               |  
0x0026 0x10      define_global                     |  
0x0027 0x0a        (constant)     "breakfast"      |  

              11 print breakfast;
0x0028 0x11      get_global                        |  
0x0029 0x0e        (constant)     "breakfast"      |  
0x002a 0x0e      print                             |  

              12 
0x002b 0x07      nil                               |  
0x002c 0x00      return                            |  
-------------------------------- execution trace -------------------------------
offset byte line meaning          encoded data     stack before inst. exec.

               1 var food = "toast";
0x0000 0x01      constant                          | [ <fn <script>> ] 
0x0001 0x01        (constant)     "toast"          | [ <fn <script>> ] 
0x0002 0x10      define_global                     | [ <fn <script>> ][ toast ] 
0x0003 0x00        (constant)     "food"           | [ <fn <script>> ][ toast ] 

               2 var beverage = "coffee";
0x0004 0x01      constant                          | [ <fn <script>> ] 
0x0005 0x03        (constant)     "coffee"         | [ <fn <script>> ] 
0x0006 0x10      define_global                     | [ <fn <script>> ][ coffee ] 
0x0007 0x02        (constant)     "beverage"       | [ <fn <script>> ][ coffee ] 

               4 var day = "saturday";
0x0008 0x01      constant                          | [ <fn <script>> ] 
0x0009 0x05        (constant)     "saturday"       | [ <fn <script>> ] 
0x000a 0x10      define_global                     | [ <fn <script>> ][ saturday ] 
0x000b 0x04        (constant)     "day"            | [ <fn <script>> ][ saturday ] 

               5 if (day == "saturday") {
0x000c 0x11      get_global                        | [ <fn <script>> ] 
0x000d 0x06        (constant)     "day"            | [ <fn <script>> ] 
0x000e 0x01      constant                          | [ <fn <script>> ][ saturday ] 
0x000f 0x07        (constant)     "saturday"       | [ <fn <script>> ][ saturday ] 
0x0010 0x0b      equal                             | [ <fn <script>> ][ saturday ][ saturday ] 
0x0011 0x18      jump_if_false    0x0011 -> 0x001d | [ <fn <script>> ][ true ] 
0x0012 0x00        (jump hi byte)                  | [ <fn <script>> ][ true ] 
0x0013 0x09        (jump lo byte)                  | [ <fn <script>> ][ true ] 
0x0014 0x0f      pop                               | [ <fn <script>> ][ true ] 

               6   food = "waffles";
0x0015 0x01      constant                          | [ <fn <script>> ] 
0x0016 0x09        (constant)     "waffles"        | [ <fn <script>> ] 
0x0017 0x12      set_global                        | [ <fn <script>> ][ waffles ] 
0x0018 0x08        (constant)     "food"           | [ <fn <script>> ][ waffles ] 
0x0019 0x0f      pop                               | [ <fn <script>> ][ waffles ] 

               7 }
0x001a 0x17      jump             0x001a -> 0x001e | [ <fn <script>> ] 
0x001b 0x00        (jump hi byte)                  | [ <fn <script>> ] 
0x001c 0x01        (jump lo byte)                  | [ <fn <script>> ] 

               9 var breakfast = food + " with " + beverage;
0x001e 0x11      get_global                        | [ <fn <script>> ] 
0x001f 0x0b        (constant)     "food"           | [ <fn <script>> ] 
0x0020 0x01      constant                          | [ <fn <script>> ][ waffles ] 
0x0021 0x0c        (constant)     " with "         | [ <fn <script>> ][ waffles ] 
0x0022 0x03      add                               | [ <fn <script>> ][ waffles ][  with  ] 
0x0023 0x11      get_global                        | [ <fn <script>> ][ waffles with  ] 
0x0024 0x0d        (constant)     "beverage"       | [ <fn <script>> ][ waffles with  ] 
0x0025 0x03      add                               | [ <fn <script>> ][ waffles with  ][ coffee ] 
0x0026 0x10      define_global                     | [ <fn <script>> ][ waffles with coffee ] 
0x0027 0x0a        (constant)     "breakfast"      | [ <fn <script>> ][ waffles with coffee ] 

              11 print breakfast;
0x0028 0x11      get_global                        | [ <fn <script>> ] 
0x0029 0x0e        (constant)     "breakfast"      | [ <fn <script>> ] 
0x002a 0x0e      print                             | [ <fn <script>> ][ waffles with coffee ] 
waffles with coffee

              12 
0x002b 0x07      nil                               | [ <fn <script>> ] 
0x002c 0x00      return                            | [ <fn <script>> ][ (nil) ] 
```
