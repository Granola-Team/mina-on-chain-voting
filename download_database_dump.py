import sys
import os
import certifi
import pycurl
from io import BytesIO

from bs4 import BeautifulSoup

from os.path import exists

def url(max_keys):
    return f'https://storage.googleapis.com/mina-archive-dumps?max-keys={max_keys}'

def get_index():
    buffer = BytesIO()
    c = pycurl.Curl()
    #initializing the request URL
    c.setopt(c.URL, f'https://storage.googleapis.com/mina-archive-dumps')
    #setting options for cURL transfer  
    c.setopt(c.WRITEDATA, buffer)
    #setting the file name holding the certificates
    c.setopt(c.CAINFO, certifi.where())
    # perform file transfer
    c.perform()
    #Ending the session and freeing the resources
    c.close()

    return buffer.getvalue().decode('iso-8859-1')

def get_sql(dump_name):
    buffer = BytesIO()
    c = pycurl.Curl()
    #initializing the request URL
    c.setopt(c.URL, f'https://storage.googleapis.com/mina-archive-dumps/{dump_name}')
    #setting options for cURL transfer  
    c.setopt(c.WRITEDATA, buffer)
    #setting the file name holding the certificates
    c.setopt(c.CAINFO, certifi.where())
    # perform file transfer
    c.perform()
    #Ending the session and freeing the resources
    c.close()

    return buffer.getvalue()

def write_sql(dump_name, dump_bytes):
    file = open(f'database_dumps/{dump_name}', "w+b")
    file.write(dump_bytes)


def main():
    soup = BeautifulSoup(get_index(), 'xml')

    sys.stderr.write("finding latest dump...\n")
    dump_name = list(soup.find_all('Key'))[-1].contents[0]

    if dump_name.endswith(".tar.gz"):
        dump_name = dump_name.replace(".tar.gz", "")

    sys.stderr.write(f'found {dump_name}\n')
    sys.stdout.write(dump_name)
    if exists(f'./database_dumps/{dump_name}.tar.gz'):
        sys.stderr.write("latest dump already downloaded, skipping...\n")
        return

    sys.stderr.write("downloading latest dump...\n")
    dump_bytes = get_sql(f'{dump_name}.tar.gz')
    
    sys.stderr.write(f'writing dump to ./database_dumps/{dump_name}.tar.gz\n')
    write_sql(f'{dump_name}.tar.gz', dump_bytes)

if __name__ == "__main__":
    main()