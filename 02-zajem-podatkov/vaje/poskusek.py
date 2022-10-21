import re
import requests

#vzorec_podatki_oglasa = re.compile(
#     r'<h3.*?name="(?P<id>\d+)" '
#     r'class="link" href="(?P<opis>.*?)"'
#     r'>(?P<ime>.*?)</a>',
#     flags=re.DOTALL
# )
#block = '<h3 class="entity-title"><a name="10124073" class="link" href="/macke-brez-rodovnika/ruska-modra-macka-oglas-10124073">Ruska Modra macka</a>'

#podatki = vzorec_podatki_oglasa.search(block).groupdict()
#podatki['id'] = int(podatki['id'])
#print(podatki)

#a = '20.2.2022'
#print(a.replace('.','. '))

URL = 'http://www.bolha.com/macke-z-rodovnikom/bengalska-macka-bengalski-mucek-z-rodovnikom-oglas-2695128'
vzorec_opis = re.compile(r'<div class="ClassifiedDetailDescription-text">(?P<opis>.*?)</div>', re.DOTALL)

def get_description_from_link(url, vzorec):
    """Funkcija vrne opis oglasa, dobljen iz povezave iz naslova oglasa"""
    try:
        # del kode, ki morda sproži napako
        status = requests.get(url)
    except Exception as e:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print(f"Napaka pri povezovanju do: {url} ::", e)
        return None
    # nadaljujemo s kodo če ni prišlo do napake
    vsebina = status.text
    opis_macke = re.findall(vzorec, vsebina)
    opis = str(re.sub(r'<br.*?\n', ' ', opis_macke[0]))
    opis = opis.replace('  ', ' ')
    return opis

print(get_description_from_link(URL, vzorec_opis))