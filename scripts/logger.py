import sys

def log(val):
    print(str(val), file=sys.stderr)

def logErr(val):
    print(str(val), file=sys.stderr)

def logOut(val):
    print(str(val), file=sys.stdout)
