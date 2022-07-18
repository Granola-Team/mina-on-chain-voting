import sys
import certifi
import pycurl
from io import BytesIO

from bs4 import BeautifulSoup

def url(max_keys):
    return f'https://storage.googleapis.com/mina-archive-dumps?max-keys={max_keys}'

def get_index(max_keys):
    buffer = BytesIO()
    c = pycurl.Curl()
    #initializing the request URL
    c.setopt(c.URL, f'https://storage.googleapis.com/mina-archive-dumps?max-keys={max_keys}')
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
    file = open(f'database_dumps/{dump_name}', "w")
    file.write(dump_bytes.decode('iso-8859-1'))


def main():
    soup = BeautifulSoup(get_index(1), 'xml')
    print("finding latest dump...")
    dump_name = soup.find('Key').contents[0]
    print(f'found {dump_name}')
    print("downloading latest dump...")
    dump_bytes = get_sql(dump_name)
    print(f'writing dump to ./database_dumps/{dump_name}')
    write_sql(dump_name, dump_bytes)

if __name__ == "__main__":
    main()