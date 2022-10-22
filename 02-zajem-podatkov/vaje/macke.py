from turtle import end_fill
import requests
import re
import os
import csv
import json

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL glavne strani bolhe za oglase z mačkami
base_url = 'http://www.bolha.com'
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = '02-zajem-podatkov\\vaje\\macke_podatki'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'index.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'macki.csv'
# ime .json datoteke v katero bomo shranili podatke
json_filename = 'macki.json'
vzorec_celega_oglasa = r'<li class=" EntityList-item EntityList-item.*?class="price price--hrk.*?</strong>'
vzorec_podatki_oglasa = re.compile(
    r'<h3.*?name="(?P<id>\d+)" '
    r'class="link" href="(?P<link_do_podrobnosti>.*?)"'
    r'>(?P<ime>.*?)</a>.*'
    r'"pubdate">(?P<datum>.*?)\.</time>.*'
    r'<strong class="price price--hrk">\s*(?P<cena>.*?)(&|\s</strong>)',
    flags=re.DOTALL)
vzorec_lokacija = re.compile(
    r'Lokacija: </span>(?P<lokacija>.*?)<br.*',
    flags=re.DOTALL)
vzorec_opis = re.compile(r'<div class="ClassifiedDetailDescription-text">(?P<opis>.*?)</div>', re.DOTALL)


def download_url_to_string(url):
    """Funkcija kot argument sprejme niz in poskusi vrniti vsebino te spletne strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    try:
        # del kode, ki morda sproži napako
        status = requests.get(url)
    except Exception as e:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print(f"Napaka pri povezovanju do: {url} ::", e)
        return None
    # nadaljujemo s kodo če ni prišlo do napake
    return status.text


def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    text = download_url_to_string(page)
    save_string_to_file(text, directory, filename)
    return None

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz."""
    pot = os.path.join(directory, filename)
    with open(pot, 'r', encoding='utf-8') as vsebina:
        return vsebina.read()


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


# Pojdi na https://regex101.com/, kjer lažje vidiš matche in potem to skopiraš v naslednjo kodo

def page_to_ads(vzorec, page_content):
    """Funkcija poišče posamezne oglase, ki se nahajajo v spletni strani in
    vrne seznam oglasov."""
    seznam_podatkov = re.findall(vzorec, page_content, re.DOTALL | re.IGNORECASE)
    return seznam_podatkov


# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, lokaciji, datumu objave in ceni v oglasu.

# Kako pobrati podatke iz več strani? pogledaš url-je in se zapelješ po for zanki ...

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

def get_dict_from_ad_block(vzorec, vzoreclok, baseurl, block):
    """Funkcija iz niza za posamezen oglasni blok izlušči podatke o imenu, datumu objave, lokaciji, ceni
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke."""
    podatki = vzorec.search(block).groupdict()
    podatki['id'] = int(podatki['id'])
    podatki['link_do_podrobnosti'] = baseurl + podatki['link_do_podrobnosti']
    # lahko tudi izpišemo celoten opis:
    podatki['opis'] = get_description_from_link(podatki['link_do_podrobnosti'], vzorec_opis)
    if podatki['cena'].isdigit() == True:
        podatki['cena'] = int(podatki['cena'])
    # v datumu damo za presledek piko, ker nimajo pojma:(
    podatki['datum'] = podatki['datum'].replace('.','. ')
    lokacija = re.search(vzoreclok, block)
    if lokacija is None:
        podatki['lokacija'] = 'Neznana'
    else:
        podatki['lokacija'] = lokacija.group('lokacija')
    return podatki

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory, vzorec, vzorecpodatki, baseurl, vzoreclok):
    """Funkcija prebere podatke v datoteki "directory"/"filename" in jih
    pretvori (razčleni) v pripadajoč seznam slovarjev za vsak oglas posebej."""
    vsebina = read_file_to_string(directory, filename)
    seznam_oglasov = page_to_ads(vzorec, vsebina)
    seznam_slovarjev_oglasov =[get_dict_from_ad_block(vzorecpodatki, vzoreclok, base_url, oglas) for oglas in seznam_oglasov]
    return(seznam_slovarjev_oglasov)


###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da so ključi vseh
    slovarjev parametra ads enaki in je seznam ads neprazen."""
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    write_csv(ads[0].keys(), ads, directory, filename)



# Windows rabi backslashe, ampak v drugih OS, to ne bo kul, zato pred tem napišemo os.sep oz. še lepše os.path.join(), ki sprejme seznam poti, ki bi jih radi združili in avtomatsko porihta stvari, kakor je treba
# os.path.join()
# folder = "pot\\do\\mape" 
# name = "a.html"

###############################################################################
# Samoiniciativa - shranimo še v .json datoteko
###############################################################################
def write_json(seznam_slovarjev, directory, filename):
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as json_file:
        json.dump(seznam_slovarjev, json_file, ensure_ascii=False, indent=4)
    return None

# Celoten program poženemo v glavni funkciji

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran (če je ful linkov, dej vmes time.stop, da program malo počaka in ne crkne)
    save_frontpage(cats_frontpage_url, cat_directory, frontpage_filename)

    # Iz lokalne (html) datoteke preberemo podatke:
    #   vsebina = read_file_to_string(cat_directory, frontpage_filename)
    # Podatke preberemo v lepšo obliko (seznam slovarjev):
    #   seznam_oglasov = page_to_ads(vzorec_celega_oglasa, vsebina)
    #   # vrne seznam posameznih oglasov
    # preverimo, ali se število matchom ujema, in se:)
    #   print(seznam_oglasov)
    #   print('Število oglasov: 'len(seznam_oglasov))
    # spremeniMO obstoječ seznam v bolj urejenega, s slovarji, v vsakem slovarju urejeni podatki oglasa:
    #   for oglas in seznam_oglasov:
    #        seznam_oglasov[seznam_oglasov.index(oglas)] = get_dict_from_ad_block(vzorec_podatki_oglasa, vzorec_lokacija, oglas)
    #   print(seznam_oglasov)
    
    # lahko pa vse to zgoraj zakomentiramo in združimo z eno funkcijo:
    seznam_slovarjev_oglasi = ads_from_file(frontpage_filename, cat_directory, vzorec_celega_oglasa, vzorec_podatki_oglasa, base_url, vzorec_lokacija)
    # dobimo seznam slovarjev s podatki o vsakem oglasu
    # print(seznam_slovarjev_oglasi)

    # Podatke shranimo v csv datoteko
    write_cat_ads_to_csv(seznam_slovarjev_oglasi, cat_directory, csv_filename)

    # Še .json datoteka za lepši slovar:
    write_json(seznam_slovarjev_oglasi, cat_directory, json_filename)

    # Dodatno: S pomočjo parametrov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prenese (četudi že obstaja)
    # in enako za pretvorbo


# ko v Pythonu poženemo/importamo datoteko, se vse požene in vse funkcije so žive. Naslednja funkcija pravi: "Če je spremenljivka imenovana 'main', potemi, da je to datoteka, s katero smo začeli, zato lahko poženeš funkcijo main(), sicer pomeni, da nas nekdo importa."
if __name__ == '__main__':
    main()
