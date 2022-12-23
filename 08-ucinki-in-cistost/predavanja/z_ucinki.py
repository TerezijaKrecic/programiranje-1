<<<<<<< HEAD
def f(sez):
    sez.append(1)
    return len(sez)

def g(sez):
    return f(sez) + f(sez)
=======
sez = [1, 2, 3]

def f(x):
    sez = sez.copy()
    sez.append(x)
    return len(sez)

def g(x):
    return f(x) + f(x)
>>>>>>> c456325d506f9e9476ca6aafba18d77c7556f801
