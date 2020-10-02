import requests
from requests.auth import AuthBase
from cryptography.hazmat.primitives.hmac import HMAC
from cryptography.hazmat.primitives.hashes import SHA256
from cryptography.hazmat.backends import default_backend
from datetime import datetime, timedelta
from time import mktime
import pandas as pd
from sys import exit
import os

# Class to perform HMAC encoding
class AuthHmacMetosGet(AuthBase):
    # Creates HMAC authorization header for Metos REST service GET request.
    def __init__(self, apiRoute, publicKey, privateKey):
        self._publicKey = publicKey
        self._privateKey = privateKey
        self._method = 'GET'
        self._apiRoute = apiRoute

    def __call__(self, request):
        dateStamp = datetime.utcnow().strftime('%a, %d %b %Y %H:%M:%S GMT')
        request.headers['Date'] = dateStamp
        msg = (self._method + self._apiRoute + dateStamp + self._publicKey).encode(encoding='utf-8')
        h = HMAC(self._privateKey.encode(encoding='utf-8'), SHA256(), backend=default_backend())
        h.update(msg)
        signature = (h.finalize()).encode('hex')
        request.headers['Authorization'] = 'hmac ' + self._publicKey + ':' + signature
        return request

"""create URL function"""
def URL_path(st_id,_from,_to,_aggr):
    _path0 = '/data/optimized/'

    Ftime = mktime(_from.timetuple())
    Ttime = mktime(_to.timetuple())

    time = 'from/' + str(Ftime) + '/to/' + str(Ttime)

    path = (_path0 + st_id + '/' + _aggr + '/' + time)
    return path

def py_api_get_st_ids(url, publicKey, privateKey):

    st_ids = []

    # Service/Route that you wish to call
    apiRoute = '/user/stations'

    # Request station ids
    auth = AuthHmacMetosGet(apiRoute, publicKey, privateKey)
    response = requests.get(url + apiRoute, headers={'Accept': 'application/json'}, auth=auth)

    # Extract to list
    json_list = response.json()
    for j in range(len(json_list)):
        st_ids.append(json_list[j]['name']['original'])

    return st_ids

def py_api_get_data(st_id, datestart, dateend,
                    url, publicKey, privateKey, time_res):

    # Construct url to download data
    url_data = URL_path(st_id, datestart, dateend, time_res)

    # Download the data
    auth = AuthHmacMetosGet(url_data, publicKey, privateKey)
    st_data = requests.get(url + url_data, headers={'Accept': 'application/json'}, auth=auth)

    status = st_data.status_code

    if status == 200:
        return st_data.json()
    else:
        return status
