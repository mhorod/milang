# mi
Small dynamically typed language made in rust created to learn basic concepts
of crafting an interpreter.

## Roadmap
### Stage 1
- [x] lexer
- [ ] parsing binary expressions made of single operator
- [ ] parsing comments
- [ ] parsing variables and functions
- [ ] validating semantics of variables and functions
- [ ] executing simple code

### Stage 2
- [ ] conditionals and while loops
- [ ] advanced expression parser
- [ ] built-in functions (such as io)

### Stage 3
- [ ] structure definitions
- [ ] `impl` blocks
- [ ] arrays and tuples
- [ ] references

### Stage 4 (perhaps)
- [ ] build-in simple testing framework
- [ ] reflection
- [ ] types


## Syntax

### Stage 1
```
# Target features for stage 1
# This code should be executable by the intepreter

# Assignments
let x = 2;
let y = 2 * x;
print(x + y); # print is a special function for now

# Function definition
fn f(a, b)
{
  return a + b;
}
print(f(x, y));
```

### Stage 2
```
# Target features for stage 2

let x = 2 + 2 * 2;
# conditional expression
if x == 6 
{
  # Print is now certified built-in function
  print("That's how it works");
} 
else if x == 8
{
  print("Wrong precedence of operations...");
}
else
{
  print("At least it's not 8, right?");
}

let y = 0;
# while loop
while y < 10 
{
  print(y ** 2);
  y += 1;
}
```

### Stage 3
```
# Target features for stage 3

# Definition of a struct 
struct Book 
{
  title,
  author,
}

# implementation of struct methods
impl Book {
  fn open(self) 
  {
    print(self.title);
  }
}

# Book constructor is a function returning the object
let book = Book("A title", "An author");
let title = book.title; # Accessing a member

# Method call is the only place where implicit reference occurs
# These two calls are equivalent
Book::open(@book);
book.open();

# Arrays don't have to be homogeneous
let array = [1, 2, "fizz", 4, "buzz"];
let tuple = ("Hello", 42);

# References
let x = 7;
let y = @x;
# Dear y, you get a dollar and you get me your stuff ~honvex-cool
$y = 8; # Dereferencing y
if x == 8 
{
  print("Reference ok");
}
else
{
  print("Reference failed to do it's job");
}

let a = 5;
let b = @a;
let c = @b;
let d = $b;
$$c = 10;
# a == 10
d = 3;
# a == 10, d did not change a
```
