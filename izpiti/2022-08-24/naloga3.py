def stevilo_nzz(n: int, k: int, l: int) -> int:
#   n = dolžina pasu
#   k = max. dolžina vzorca (min. je 1)   
#   l = min. razdalja med 2 vzorcema
#   namesto X _ _ _ pišem 1 0 0 0
    stevec = 1 # prazen pas je vedno opcija
    for dolzina in range(1, k + 1): # za vsako dolžino vzorca preštejemo primere
        index = 0
        sez = []
        # tukaj se nekaj zgodi, še ena zanka, na koncu vsake zanke se prišteje +1 k števcu
        na_prvem = [1 for _ in range(dolzina)] 

