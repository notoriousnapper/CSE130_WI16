from misc import Failure

# Function isSeq is a helper function determines if object is argument is of 
# type sequence. Returns True or False

def isSeq(object):
    return type(object) in [str, unicode, tuple, list, bytearray, xrange, buffer]

# Vector class allows for creation of.innertor objects, which contain a sequence 
# of values.  Operations between.innertors are also also accounte for, such as
# addition.
class Vector(object):

# Function init is a contructor for the.innertor class that creates a.innertor 
# object. If the object has int or long, create a.innertor with length of
# the object. 
# 
# The field variable "inner" is the vector
    def __init__(self, object):
        if (isinstance(object,int) or isinstance(object,long)):
            if (object < 0):
                raise ValueError("Vector length cannot be negative")
            self.inner = [0.0 for i in range(object)]
        elif (isSeq(object)):
            self.inner = list(object)
        else:
            raise TypeError("Unexpected type %s" % type(object))

# Function repr returns a string representation of the vector
    def __repr__(self):
        res =  repr(self.inner) 
	return "Vector(" + res + ")"

# Function len returns the length of the inner object
    def __len__(self):
	return len(self.inner)
# Function iter returns an iterable object of a given vector
    def __iter__(self):
   	for n in self.inner: 
	    yield(n)
# Function (+) will add two vector objects inner values, to create a new vector
# reflecting all the additions
    def __add__(self, other):
        if (len(self) != len(other)):
            raise ValueError("Cannot add.innertors of different lengths")
        return Vector([(i + j) for (i,j) in (zip(list(self), list(other))) ])
# 2nd Situation where Function (+) will add two vector objects inner values, to create a new vector
# reflecting all the additions
    def __radd__(self, other):
        if (len(self) != len(other)):
            raise ValueError("Cannot add.innertors of different lengths")
        return Vector([(i + j) for (i,j) in (zip(list(self), list(other))) ])

# Function (+=) will add two vector objects inner values, to essentially 
# increment an existing vectors components by the argument's inner amounts
    def __iadd__(self, other):
        if (len(self) != len(other)):
            raise ValueError("Cannot add.innertors of different lengths")
        self.inner = Vector([(i + j) for (i,j) in (zip(list(self), list(other))) ])
        return self.inner
# Function dot will give the dot product of a given vector, and an argument vector
# as an int.  Done by zipping self and other and iterating over them to create a list
# of the products of their members
    def dot(self, other):
        if (len(self) != len(other)):
            raise ValueError("Cannot perform operation on.innertors of different lengths")
        try:
            return sum( [i * j for i,j in zip(self, other)] )
        except:
            return sum( [str(i) + str(j) for i,j in zip(self, other)] )


# Function getitem returns the vector objects element specified by key
# Python's Accessor method demo
    def __getitem__(self, key):
	length = len(self)
        if (type(key) == slice):
            return self.inner[key]
        else:
            if (key >= length or key < -length):
                raise IndexError("vector index out of range")
            else:
                if (key < 0):
                    key += length(self)
                return self.inner[key]


# Function setitem replaces an item specified by a key argument, to whatever value
# The use demands
# Python's mutator method demo
    def __setitem__(self, key, value):
        if (type(key) == slice):
            temp = [x for x in self.inner]
            temp[key] = value

            if (len(temp) != len(self.inner)):
                raise ValueError("cannot change vector length")
            else:
                self.inner = temp
        else:
            if (key >= len(self) or key < -len(self)):
                raise IndexError("vector index out of range")
            self.inner[key] = value



# Python Operators
# Function "=", if vector have same elements in same order, return True
    def __eq__(self, other):
        if isinstance(other, Vector):
            for i, j in zip(self, other):
                if not i == j:
                    return False
            return True
        else:
            return False

# Function "!=", if vector have same elements in same order, return False
    def __ne__(self, other):
        return not self.__eq__(other)

# Function ">", if self.inner's largest element is greater than the the
# largest element of other, return True
    def __gt__(self, other):
        if isinstance(other, Vector):
            for i, j in zip(sorted(self, reverse = True), 
                            sorted(other, reverse = True)):
                if not i > j:
                    return False
            return True
        else:
            return False

    def __ge__(self, other):
        if isinstance(other, Vector):
            for i, j in zip(sorted(self, reverse = True), 
                            sorted(other, reverse = True)):
                if not i >= j:
                    return False
            return True
        else:
            return False

    def __lt__(self, other):
        return not self.__ge__(other)

    def __le__(self, other):
        return not self.__gt__(other)
