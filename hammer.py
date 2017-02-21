import requests
import requests.packages.urllib3
import time

requests.packages.urllib3.disable_warnings()

for x in range(1, 500):
    start = time.time()
    r = requests.get('https://159.203.231.197/new-customers/', verify=False, auth=('', ''))
    j = requests.get('https://159.203.231.197/new-customer/58bfd58c-94eb-47c5-8b10-2cb8cc120f95', verify=False, auth=('', ''))
    print x, r, j, time.time() - start
