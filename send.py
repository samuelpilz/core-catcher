import requests
import json

url = 'https://coveralls.io/api/v1/jobs'

req = requests.post(url, files={'json_file': open('json.file', 'rb')})

print(req.status_code)
print(req.text)
