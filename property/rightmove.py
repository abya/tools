from lxml import html
import requests
import urllib2

page = requests.get('http://www.rightmove.co.uk/property-for-sale/Slough.html')
tree = html.fromstring(page.text)

nodes = tree.xpath('//div[@class="details clearfix"]')
for x in nodes:
  res = "http://www.rightmove.co.uk" + x.xpath('p/a[@class="more-details"]/@href')[0]
  req = urllib2.Request(res, headers={ 'User-Agent': 'Mozilla/5.0' })
  domtree = html.fromstring(urllib2.urlopen(req).read())
  details = domtree.xpath('//div[@class="propertyDetailDescription"]/text()')
  print res, details

