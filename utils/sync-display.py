#!/usr/bin/env python3
import urllib.request
import time

with open("host.txt", "r") as hostFile:
    host = hostFile.read()

while True:
    with urllib.request.urlopen(host) as response:
        with open("status.txt", "wb") as statusFile:
            statusFile.write(response.read())
    print("%s: Updated status" % time.strftime("%c"))
    time.sleep(3)
