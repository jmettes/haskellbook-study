# try and make a Y Combinator in python
# this doesn't work, because it's not lazily evaluated
# it throws: 'RuntimeError: maximum recursion depth exceeded'

def fact(n):
    return recurse(factprime)(n)

def factprime(f, n):
    if n == 0:
        return 1
    else:
        return n * f(n - 1)

def recurse(f):
    # infinite loop... not lazy evaluated
    return f(recurse (f))

# this will throw recursion depth error
# fact(5)


# the following will work however!

fac = lambda f: lambda n: (1 if n == 0 else n * f(n - 1))
Y = lambda f: lambda n: f(Y(f))(n)

print Y(fac)(5)
