# -*- coding: utf-8 -*-
#autolisp local value checker

from __future__ import division
import math
import operator as op
import functools as fc
import re

################ Types

Symbol = str          # A Lisp Symbol is implemented as a Python str
List   = list         # A Lisp List is implemented as a Python list
Number = (int, float) # A Lisp Number is implemented as a Python int or float

################ Parsing: parse, tokenize, and read_from_tokens

def parse(program):
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(program))
def convertStringToParenthese(s):
    pattern= re.sub(r"('.*?')","\1",s)
def tokenize(s):
    #remove comment    
    s+="\n"
    s=re.sub(r";.*?\n","",s)    
    "Convert a '(lambda to (lambda"    
    s=s.replace("'(lambda","(lambda")
    
    "Convert a '( to (list"    
    s=s.replace("'(","(list")
    
    
    s=s.replace('\\"',"")
    s=re.sub(r'("(.*?)")',r"string",s)
    
    "Convert a string into a list of tokens."
    s=s.replace('(',' ( ').replace(')',' ) ')    
    s="( defun global ( ) "+" ( progn "+s+" ) )"        
    return s.split()
def read_from_tokens(tokens):
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:                
        return atom(token)

def atom(token):
    "Numbers become numbers; every other token is a symbol."
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:                  
            return Symbol(token)

################ Environments
DEFAULTSET={
        'string',
        "pi",
        "PI",
        "nil",
        "NIL",
        "t",
        "T",
        ".",
        "acAllViewports",
    }
def standard_env():        
    "An environment with some Scheme standard procedures."
    env = Env()    
    return env

class Env(set):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), outer=None):        
        self.update(parms)
        self.outer = outer
    def find(self, var):
        "Find the innermost Env where var appears."           
        if var in DEFAULTSET:
            return ""    
        if (var in self) == False:            
            if self.outer != None:                
                ret=self.outer.find(var)    
                if ret != None:
                    return "outer"    
                else:
                    return None    
            else:
                return None
        else:
            return ""

global_env = standard_env()

################ Interaction: A REPL

def replFromFile(filename):    
    content=""
    with open(filename,"r",encoding="utf-8") as f:
        content=f.read()            
    repl(content)
def repl(content):    
    val = eval(parse(content))    

def lispstr(exp):
    "Convert a Python object back into a Lisp-readable string."
    if isinstance(exp, List):
        return '(' + ' '.join(map(lispstr, exp)) + ')' 
    else:
        return str(exp)

################ Procedures

class Procedure(object):
    "A user-defined Scheme procedure."
    def __init__(self, parms, body, env):
        self.parms, self.body, self.env = parms, body, env
    def __call__(self, *args): 
        return 

################ eval
def eval(x, env=global_env):
    "Evaluate an expression in an environment."
    if isinstance(x, Symbol):      # variable reference    
        if x[0] == "*" and x[-1] == "*": #global *name* is dismissed
            return [],[]
        if x[0] == "'": #quoto is dismissed
            return [],[]
        t=env.find(x)
        if t == None:
            return [x],[]
        elif t == "outer":#outer reference
            return [],[x]
        else:
            return [],[]    
    elif not isinstance(x, List):  # constant literal
        return [],[]     
    elif x[0] == 'lambda':        #lambda
        parms=x[1]
        body=x[2:]        
        ret=[]
        retout=[]                
        for t in body:            
            t1,t2=eval(t, Env(parms, env))
            ret.extend(t1)
            retout.extend(t2)
        if len(ret) != 0:
            print("defun lambda:"+str(set(ret))+"are not defined as local values.")
        if len(retout) != 0:
            print("defun lambda:"+str(set(retout))+"refers to outside values.")    
        return [],[]
    elif x[0] == 'defun':            
        funcname=x[1]
        parms=x[2]
        body=x[3:]        
        env.update({funcname:funcname})        
        ret=[]
        retout=[]
        for t in body:
            t1,t2=eval(t, Env(parms, env))
            ret.extend(t1)
            retout.extend(t2)
        if len(ret) != 0:
            print("defun "+funcname+":"+str(set(ret))+"are not defined as local values.")
        if len(retout) != 0:
            print("defun "+funcname+":"+str(set(retout))+"refers to outside values.")
        return [],[]
    else:                          # (proc arg...)
        ret=[]        
        retout=[]        
        for i in range(1,len(x)):    
            t1,t2=eval(x[i], env)                    
            ret.extend(t1)
            retout.extend(t2)
        return ret,retout

if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 2:
        filenames=sys.argv[1:]
        for file in filenames:
            print()    
            print("process "+file)        
            print()    
            replFromFile(file)    
    else:
        print("specify filenames as arguments.")    
    input("input something：")
