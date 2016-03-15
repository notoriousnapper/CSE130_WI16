from misc import *
import crypt
import re

# Function Name: load_words(filename, regexp)
# Function Purpose: Loads the words from the file filename which match the regular 
# expression regexp. There will be one word per line in the input file. The 
# resulting words should be returned in a list in the same order they occur in 
# the input file.
def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""

    f = open(filename, 'r')
    r = re.compile(regexp)
    a = []
    for line in f.readlines():
        word = line.strip()
        if r.match(word):
            a.append(word)
    return a


#Function Name: transform_reverse(str) 
#               transform_capitalize(str) 
#     	  transform_digits(str)
# Reverses a string, returns all combinations of uppercase,
# And returns all combinations of digits
def transform_reverse(str):
    rev = str[::-1]
    return [str,rev]

def transform_capitalize(str):
    #First, set everything small, so that you can just do all combos of caps
    retStr = str.upper()

    for i in range ( 1 < len(retStr)):
        l = [str.lower()]
        word = list(str.lower())
        for j in range ( 0 < len(retStr)):
            if i & 1 << j:
                word[j:j+1] = list((''.join(new_word[j:j+1])).upper())
                l.append(''.join(new_word))
        return l



#  helper Function
def helper(s, n, i):
    s = s[:index]
    s += s(n) + s[i+1:]
    return s

def transform_digits(str):
#Cases
#a-4, b-6,8
#e-3, #g-9
#i-1, #l -1
#o-0, #q-9
#s-5, #t-7
#z-2
    resStr = [str]
    for word in res:
        i = 0
  	while i < len(word): 
            lowerC = word[i].lower() 
            s_word = word 
 
            if lowerC == 'a': 
                s_word = helper(s_word,'4',i) 
            elif lowerC == 'b': 
                s_word = helper(s_word,'6',i) 
            elif lowerC == 'e': 
                s_word = helper(s_word,'3',i) 
            elif lowerC == 'g': 
                s_word = helper(s_word,'9',i) 
            elif lowerC == 'i': 
                s_word = helper(s_word,'1',i) 
            elif lowerC == 'l': 
                s_word = helper(s_word,'1',i) 
                if (s_word not in resStr): 
                    resStr.append(s_word) 
                s_word = helper(s_word,'8',i) 
            elif lowerC == 'o': 
                s_word = helper(s_word,'0',i) 
            elif lowerC == 'q': 
                s_word = helper(s_word,'9',i) 
            elif lowerC == 's': 
                s_word = helper(s_word,'5',i) 
            elif lowerC == 't': 
                s_word = helper(s_word,'7',i) 
            elif lowerC == 'z': 
                s_word = helper(s_word,'2',i) 
            else: pass 
            if (s_word not in resStr): 
                resStr.append(s_word) 
            i += 1 
    return resStr 


# Function Name: check_pass(plain, enc)
#"""Check to see if the plaintext plain encrypts to the encrypted 
def check_pass(plain,enc): 
    bool = crypt.crypt(plain, enc[0:2]) == enc 
    return bool 
 
 
 
# Function Name: crack_pass_file(filename, word, out)
# Function Purpose: Takes two strings corresponding to a password file and a 
#	file with a list of words and a string corresponding to an output
#        file. Attempt to crack as many of the passwords in the password 
#        file as possible.
# * Algorithm: 
# */
def load_passwd(filename): 
 
    l = [] 
 
    for line in f.readlines(): 
        tokens = re.split(':', line) 
        dictionary = dict(zip( ['account', 'password', 'UID', 'GID', 'GECOS', 'directory', 'shell'], 
                      tokens )) 
        l.append(dictionary) 
 
    # Close everything 
    f.close() 
    return l 
     
 
 
 
 
def crack_pass_file(pass_filename,words_filename,out_filename): 
    """Crack as many passwords in file fn_pass as possible using words 
       in the file words""" 
 
    # open output file 
    ostream = open(out_filename, 'w') 
 
    accs = load_passwd(pass_filename) 
    words = load_words(words_filename, r'^.{6,8}$') 
 
    for acc in accs: 
        user = acc['acc'] 
        enc = acc['password'] 
        for w in ws: 
            if check_pass(w, enc): 
                accs.remove(acc) 
                ostream.write(user + "=" + w + "\n") 
                ostream.flush() 
                break 


    for w in ws: 
        for c in transform_capitalize(w): 
            for d in transform_digits(c): 
 
                for r in transform_reverse(d): 
                    for acc in accs: 
                        user = acc['acc'] 
                        enc = acc['passw'] 
                        if check_pass(r, enc): 
                            accs.remove(acc) 
 
                            #OStream Write and clean 
                            ostream.write(user + "=" + r + "\n") 
                            ostream.flush() 
                            break 
 
    ostream.close() 


