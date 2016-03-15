 #PA6 - decorators.py
from misc import Failure

class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__func=f
        self.__name____=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__func(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0

# Class that prints an ASCII tree of the call stack
# Also, it returns decorated functions values
class traced(object):
    __lvl = 0
    def __init__(self,f):
        self.__func = f
        self.__name__ = f.__name__
    def __call__(self, *args, **dargs):
	# Print appropriate strings for tree by level
        res = "| " * traced.__lvl
        res += ",- "
        res += self.__name__ + "("
	# "repr" module provides a means for producing object representations with limits 
	# on the size of the resulting strings. This is used in the 
	# Python debugger and may be useful in other contexts as well.
        res += ", ".join([repr(arg) for arg in args])
        res += ", ".join([str(key) + "=" + repr(value) 
	    for key, value in dargs.items()])

        print(res + ')')
        traced.__lvl += 1

        try:
            res = self.__func(*args, **dargs)
            traced.__lvl -= 1

            res = "| " * traced.__lvl
            res += "`- "
            print(res + repr(res))
            return res

        except Exception, e:
            traced.__lvl -= 1
            raise e


# Class that memoizes, in that it caches return values of functions
# in the stack, so it returns functions faster after the first call
class memoized(object):
    def __init__(self,f):
        self.__func = f
        self.__cache = {}
        self.__name__ = f.__name__
    def __call__(self, *args, **dargs):
	arg1 = str(args)
	arg2 = str(dargs)
        line = arg1 + arg2
        if line not in self.__cache:
            try:
                self.__cache[line] = self.__func(*args,**dargs)
            except Exception as inst:
                self.__cache[line] = inst

        if isinstance(self.__cache[line], Exception):
            raise self.__cache[line]
        return self.__cache[line]

# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print "RUNNING %s(%s):" % (f.__name____,", ".join([repr(x) for x in a]))
        rv=f(*a)
        print "RETURNED %s" % repr(rv)

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)



