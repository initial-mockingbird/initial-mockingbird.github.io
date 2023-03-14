---
title: "A Naive approach to Functional Programming in C! "
date: 2023-03-14 05:15:00 -0400
categories: [C]
tags: [C,functional programming, defunctionalization, higher order functions, generics, parametric polymorphism, polymorphism]
---

## Motivation

As per usual, every post should come with a motivation behind it! Something to solve. But this post is a little different.

You see, C is a low-level language. It's lightweight, handles memory explicitly, and has very little overhead. Which makes it perfect for situations where memory constraints are key, such as embedding programming.

One could say that if you are not taking advantage of the minimalistic C interface, then you shouldn't be using C. And that's mostly right. Nevertheless, there is a very specific instance in which having high abstractions in C might be very desirable: college.

When I first touched C it was in my Operating Systems course. And most of the assignments were about threads and shared state, which was ultimately abstracted as a series of message queues that can be filtered.

Moreover, in some cases, after filtering the data, we either had to reduce it to a single value, or map the values and then do some IO. And remember, this same pattern was used among different projects with different input types.

So, it became desirable to have some functional utilities, not because it was impossible to do these things without them (one could always just write procedural code that does a specific filter, or map), but rather because it bettered my personal the developer experience (and allowed for code reuse!).

## A simplified problem

For now, let's tackle a simplified version of what we want to achieve, and then we can move on from there. Let's say that we have a list of a specific type: `int`, and that we want to make a `map` function over it, such that the function it takes returns an `int`:

```c++
typedef struct _node {
  int val;
  struct _node * next;
} node;

const node* nil = NULL;

node* map(? f, node* xs){
  if (xs == nil)
    return nil;
  
  int y    = f(xs->val);
  int ys   = map(f,xs->next);
  node* l  = (node*) malloc(sizeof(node));
  l->val   = y;
  l->next  = ys;
  return l;
}
```

But what type would `f` be? One option is to let it be a function pointer, and thus:

```c
typedef struct _list {
  int head;
  struct _list * tail;
} list;

const list* nil = NULL;

list* map(int (*f)(int), list* xs){
  if (xs == nil)
    return xs;
  
  int y    = f(xs->head);
  list* ys = map(f,xs->tail);
  list* l  = (list*) malloc(sizeof(list));
  l->head   = y;
  l->tail  = ys;
  return l;
}
```

And it works like a charm! Sadly, a map that can only take a list of `int` and return another list of `int` isn't super useful. So our next step is to add a bit of generics in order for it to be a tad more polymorphic.

## Templates, but unga unga

C is very minimalistic, and doesn't really have many ways of defining polymorphism. In fact, the only way that I know of defining generics is via macros. Those that come from C++ will definitely feel a familiar vibe. Since the ideas are the same (but C macros aren't as powerful as C++ templates!)



```C
/** We define a macro that given a type, expands a definition
/* of a list for that type. Notice that the custom type is called:
/* list__<typename>, this way we don't have any clash in the name.
**/
#define mk_list(T)            \
  typedef struct _list__##T { \
    T head;                   \
    struct _list__##T *tail;  \
  } list__##T;                \

/**
/* We also define a nice alias in order to avoid writing list__<typename>
/* every time we want a list, hiding the implementation details
**/
#define list(T) list__##T

/**
/* We can then start defining functions that can interact with our list type
**/
#define mk_head(T)              \
  T head__##T(list(T) *list){   \
    return list->head;          \
  }                             \

#define mk_tail(T)                      \
  list(T)* tail__##T(list(T) *list){    \
    return list->tail;                  \
  }                                     \

#define mk_cons(T)                                          \
  list(T)* cons__##T(T x, list(T) *xs){                     \
    list(T)* new_head = (list(T)*) malloc(sizeof(list(T))); \
    new_head->head = x;                                     \
    new_head->tail = xs;                                    \
    return new_head;                                        \
  }                                                         \

// Like list(T), nice aliases that hide the implementation name.
#define head(T,xs)   head__##T(xs)
#define tail(T,xs)   tail__##T(xs)
#define cons(T,x,xs) cons__##T(x,xs)

/** We can also expose a macro that defines the whole interface 
/* automatically for better quality of life. 
**/
#define list_interface(T) \
  mk_list(T) \
  mk_head(T) \
  mk_tail(T) \
  mk_cons(T) \

// now we can make lists for all these types!
list_interface(int)
list_interface(float)
list_interface(char)
// even lists of lists!
list_interface(list(int))
```

All seems well, we now have a way of constructing generic lists. We could also define a more generic map function that returns a different type argument:

```C
#define mk_map(A,B)                                      \
  list(B)* map__##A##__##B(B (*f)(A), list(A)* xs){      \
    if (xs == NULL)                                      \
      return NULL;                                       \
    B y         = f(xs->head);                           \
    list(B)* ys = map__##A##__##B(f,xs->tail);           \
    list(B)* l  = (list(B)*) malloc(sizeof(list(B)));    \
    l->head   = y;                                       \
    l->tail   = ys;                                      \
    return l;                                            \
  }                                                      \
```

Although somewhat convenient, this way of making map functions suffers from 2 issues: the first one is that we would have to define NM different instances

```C
mk_map(int,int)
mk_map(int,float)
mk_map(int,char)
mk_map(float,float)
mk_map(float,int)
mk_map(float,char)
mk_map(char,char)
mk_map(char,int)
mk_map(char,float)
```

And the second one is that we have no way of partially applying functions.
Yet this is a nice alternative if the project is small enough.

## Enter: Defunctionalization

An alternative way of providing higher order functions is via defunctionalization. A technique which allows to embed a higher order language into a lower order one via writing an initial interpreter.

Let's say that, we want to support the following functions (and just these operations) for our higher order functions: Add, Multiply and Compose.

Then, we can mimic a tagged union that represents our operations:

```C
// We call it lambda because of lambda functions!
enum lambda_option{ Add, O, Mul };

typedef struct lambda     
{                                  
  enum lambda_option chosen;       
  union{                           
    struct                         
    {                              
      int x;                       
    } Add;                         
    struct
    {                        
      struct lambda* l;            
      struct lambda* r;            
    } o;                           
    struct
    {
      int x;
    }Mul;          
  };                               
}lambda;

// \x -> x + 1
lambda* plus1 = (lambda*) malloc(sizeof(lambda));
plus1->chosen = Add;
plus1->Add.x  = 1;

// \x -> x * 2
lambda* times2 = (lambda*) malloc(sizeof(lambda));
times2->chosen = Mul;
times2->Mul.x  = 2;

// \x -> x + 3
lambda* plus3 = (lambda*) malloc(sizeof(lambda));
plus3->chosen = Add;
plus3->Add.x  = 3;

// (*2) . (+1)
lambda* combined1 = (lambda*) malloc(sizeof(lambda));
combined1->chosen = O;
combined1->o.l = times2;
combined1->o.r = plus1;

// (+3) . (*2) . (+1)
lambda* combined2 = (lambda*) malloc(sizeof(lambda));
combined2->o.l = plus3;
combined2->o.r = combined1;
```

Not exactly the most comfortable syntax, but hey, we got a monomorphic way of partially applying a function. Now, we can write an interpreter that
evals the function:

```C
int apply(lambda* f, int x){               
  switch (f->chosen)                            
  {                                             
  case Add:                                     
    return f->Add.x + x;                        
  case Mul:                                     
    return f->Mul.x * x;                                  
  case O:                                                
    return apply(f->o.l,apply(f->o.r,x));                  
  default:                                      
    return 0;                    
  }                                             
}
```

And notice that now we can eval our lambdas:

```C
printf("%d\n",apply(combined2,5)); // outputs (5 + 1) * 2 + 3 = 15
```

If we wanted to make apply polymorphic, then a problem arises, what if we
want to have a function whose return type is different from the rest?

One option would be to have a different lambda for every different type pair (A,B). But this doesn't seem feasible. Having higher order functions this way would be nearly impossible. Which lambda should be pass? Should we also create a different map for every possible lambda?

Another option would be returning an un/tagged union. The choice of having a tag or not would depend on how we would like to handle errors: should we
verify the types through every step of the process yielding an explicit error? Then a tag is desired. Otherwise we can just use an untagged union:

```C
enum lambda_option{ Add, O, Mul, ConstC };

typedef struct lambda     
{                                  
  enum lambda_option chosen;       
  union{                           
    struct                         
    {                              
      int x;                       
    } Add;                         
    struct
    {                        
      struct lambda* l;            
      struct lambda* r;            
    } o;                           
    struct
    {
      int x;
    }Mul;          
  };                               
}lambda;


union lambda_res
{
  int  x;
  char y;
};


union lambda_res apply(lambda* f, union lambda_res x){
  union lambda_res lr;           
  switch (f->chosen)                            
  {                                             
  case Add:
    lr.x = f->Add.x + x.x; 
    return lr;
  case Mul:
    lr.x = f->Mul.x * x.x;
    return lr;                            
  case O:                                             
    return apply(f->o.l,apply(f->o.r,x));   
  case ConstC:
    lr.y = '1';
    return lr;                 
  default:                                      
    return lr;                    
  }                                             
}  

// \x -> x + 1
lambda* plus1 = (lambda*) malloc(sizeof(lambda));
plus1->chosen = Add;
plus1->Add.x  = 1;

// \x -> x * 2
lambda* times2 = (lambda*) malloc(sizeof(lambda));
times2->chosen = Mul;
times2->Mul.x  = 2;

// \x -> x + 3
lambda* plus3 = (lambda*) malloc(sizeof(lambda));
plus3->chosen = Add;
plus3->Add.x  = 3;


lambda* cc = (lambda*) malloc(sizeof(lambda));
cc->chosen = ConstC;

// (*2) . (+1)
lambda* combined1 = (lambda*) malloc(sizeof(lambda));
combined1->chosen = O;
combined1->o.l = times2;
combined1->o.r = plus1;



// (+3) . (*2) . (+1)
lambda* combined2 = (lambda*) malloc(sizeof(lambda));
combined2->chosen = O;
combined2->o.l = plus3;
combined2->o.r = combined1;


union lambda_res x;
x.x = 5;

// still 15
printf("%d\n",apply(combined2,x));
```

Moreover, if we use tagged unions, we can add a polymorphic layer on top:

```C
enum lambda_res_option{ Integer, Character, Arg, Result };
#define mk_lambda_rest(A,B)                       \
typedef struct lambda_res##A##B                   \
{                                                 \
  enum lambda_res_option lro;                     \
  union                                           \
  {                                               \
    struct                                        \
    {                                             \
      int x;                                      \
    }Integer;                                     \
    struct                                        \
    {                                             \
      char x;                                     \
    }Character;                                   \
    struct                                        \
    {                                             \
      A x;                                        \
    }Arg;                                         \
    struct                                        \
    {                                             \
      B x;                                        \
    }Result;                                      \
  };                                              \
}lambda_res##A##B;                                \



#define mk_apply(A,B)                                  \
lambda_res##A##B apply##A##B(lambda* f, lambda_res##A##B x){ \
  lambda_res##A##B lr;                                 \
  switch (f->chosen)                                   \
  {                                                    \
  case Add:                                            \
    lr.lro   = Integer;                                \
    lr.Integer.x = f->Add.x + x.Integer.x;             \
    return lr;                                         \
  case Mul:                                            \
    lr.lro   = Integer;                                \
    lr.Integer.x = f->Mul.x * x.Integer.x;             \
    return lr;                                         \
  case O:                                              \
    return apply##A##B(f->o.l,apply##A##B(f->o.r,x));  \
  case ConstC:                                         \
    lr.lro = Character;                                \
    lr.Character.x   = '1';                            \
    return lr;                                         \
  default:                                             \
    return lr;                                         \
  }                                                    \
}                                                      \


#define mk_higher_order(A,B)\
  mk_lambda_rest(A,B)\
  mk_apply(A,B)\

#define lambda_res(A,B) lambda_res##A##B    
#define apply(A,B,f,x) apply##A##B(f,x)
```

Which _might_ be useful in some cases... (maybe creating multiple `apply` functions, giving different interpretations for each result). But as far as I know, this will mostly defeat the purpose of defunctionalization (which is: knowing the functions beforehand and encode monomorphic versions of them). And also adds a couple of issues (such as we need to know the output type in order to create an input of type `lambda_res`).

## Final thoughts

By now, we have explored two of ways of defining monomorphic and (parametric) polymorphic structures/functions, coupled with higher order functions: one that uses function pointers, and one that uses defunctionalization. Nevertheless, both approaches have some drawbacks:

- The function pointers approach doesn't allow for partial application. It is too strict and has a NXM instances issue.
- Defunctionalization is very flexible, allowing us to write multiple interpreters for the same set of functions (effectively expressing a DSL by making multiple apply functions). Nevertheless, we have encoded it using an initial method. And thus, we have to explicitly manage the union types and the implementation, which makes it hard to extend. A better approach would be encoding the options using an open-union instead of a closed one, and implement a visitor pattern. But this is material for another post!