import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'cats_data'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'cat_data.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        print('Shranjujem')
        sys.stdout.flush()
        # del kode, ki morda sproži napako
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print('Could not acces page ' + url)
        return ''
    # nadaljujemo s kodo če ni prišlo do napake
    if r.status_code == requests.codes.ok:

        return r.text
    else:
        print("failed to download url " + url)
        return

def save_string_to_file(text, cat_directory, frontpage_filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(cat_directory, frontpage_filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage():
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    url = "http://www.bolha.com/zivali/male-zivali/macke/"
    save_string_to_file(download_url_to_string(url), cat_directory, frontpage_filename)
    return None

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    with open filename as vhodna:
        vsebina = vhodna.read()
    return vsebina

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.



def page_to_ads(vsebina_strani):
    '''Split "page" to a list of advertisement blocks.'''
    vzorec = re.compile(r'<div class="ad">(.*?)<div class="clear">', re.DOTALL)

    seznam = []
    for ujemanje in re.finditer(vzorec, vsebina_strani):
        seznam.append(ujemanje.group(0))

    return seznam

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.


def get_dict_from_ad_block(block):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    rx = re.compile(r'title="(?P<name>.*?)"'
                    r'.*?</h3>\s*(?P<description>.*?)\s*</?div'
                    r'.*?class="price">(?P<price>.*?)</div',
                    re.DOTALL)
    data = re.search(rx, block)
    ad_dict = data.groupdict()
    return ad_dict

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory):
    '''Parse the ads in filename/directory into a dictionary list.'''
    page = read_file_to_string(filename, directory)
    blocks = page_to_ads(page)
    ads = [get_dict_from_ad_block(block) for block in blocks]
    return ads

def ads_frontpage():
    return ads_from_file(cat_directory, frontpage_filename)

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.

def write_cat_ads_to_csv(ads, directory, filename):
    '''Write a CSV file containing one ad from "ads" on each row.'''
    write_csv(ads[0].keys(), ads, directory, filename)


def write_cat_csv(ads):
    '''Save "ads" to "cat_directory"/"csv_filename"'''
    write_cat_ads_to_csv(ads, cat_directory, csv_filename)
