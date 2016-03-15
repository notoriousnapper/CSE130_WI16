#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions
#* Function Name: closest_to
#* Function Purpose:  Returns the element of the list l closest 
# * in value to v. In the case of a tie, the first such element is returned. 
# * If l is empty, None is returned. Once implemented you should get the 
# * following behavior at the Python prompt
# * Algorithm: 
# */

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if (len(l) == 0): 
	return None

    diff = float("inf")#If Difference is 0, then you've found the element
    ret = l[0] 
    temp = 0
    for val in l:
        temp = abs(v - val)
	if (temp < diff):
	    diff = temp
	    ret = val
    return ret

#Testing
print closest_to([2,4,8,9],7)
print closest_to([2,4,8,9],5)
    
    #raise Failure("to be written")

#* Function Name: make_dict
#* Function Purpose:  Takes a list of keys and a list of values and returns a 
#* dictionary (dict) pairing keys to corresponding values.
#* Algorithm: 
#*)



def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    return dict(zip(keys, values))
	


#
#/* Function Name: word_count
# * Function Purpose:  Takes a string, filename, and returns a dictionary mapping 
#	words to the number of times they occur in the file filename.
# * Algorithm: 
# */

def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    #Open the file
    f = open(fn, 'r')
    d = dict()
    pattern = re.compile('[A-Za-z0-9_]+')
    words = "".join([c if pattern.match(c) else ' ' for c in f.read().lower()])
    for w in words.split():
        if w in d:
            d[w] += 1
        else:
            d[w] = 1
    f.close()
    return d






