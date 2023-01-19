breg1, breg2 = (
    [
        [60, 50, 40, 30, 20],
        [40, 50, 60, 73, 80],
        [10, 20, 30, 40, 50],
    ],
    [
        [30, 40, 50, 60, 70],
        [40, 60, 30, 20, 40],
        [10, 20, 90, 40, 50],
    ],
)

# prehod med bregovoma je le en, tako da to lahko razčlenimo na 2 podproblema

# najprej poiščimo, koliko energije max. mu ostane na prvem bregu (pot od 10 do 20 zgoraj desno)

def energija_prvi_breg(breg):
    sirina = len(breg[0]) - 1 # maks. indeks stolpca
    visina = len(breg) - 1 # makx. indeks vrstice
    start = breg[visina][0] # spodaj levo = 10
    def sprehod(e, x, y): # e = energija na tem mestu, x = pozicija po stolpcih, y = pozicija po vrsticah
        if (x, y) == (sirina, 0):
            return e
        else:
            korak_desno =
            if x + 1 > sirina:
                0
            else: e - 10 + sprehod()
            korak_navzgor
            korak_diag
            return max (korak_desno, korak_navzgor, korak_diag)
    return sprehod(start, 0, visina)