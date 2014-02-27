import urllib2
import csv

fileUrl = 'http://data.gov.in/access-point-download-count?url=http://data.gov.in/sites/default/files/crcCAW.csv&nid=25941'
try:
    csvFile = urllib2.urlopen(fileUrl)
    if csvFile.getcode() == 200:
        content = csvFile.read()
        with open('C:/Users/aakash/Google Drive/R/Crime/data/file.csv','wb') as out:
            out.write(content)
            out.close()
            print 'File Downloaded Sucessfully!'
            with open("C:/Users/aakash/Google Drive/R/Crime/data/file.csv","rb") as f:
                reader = csv.reader(f)
                for r in reader:
                    if not 'TOTAL (UTs)' in r:
                        if not 'TOTAL (STATES)' in r:
                            if not 'TOTAL (ALL-INDIA)' in r:
                                if not 'TOTAL CRIMES AGAINST WOMEN' in r:
                                    with open('C:/Users/aakash/Google Drive/R/Crime/data/crimeAgainstWomen.csv', 'a') as f:
                                        wr = csv.writer(f, lineterminator='\n')
                                        wr.writerow(r)
                                    f.close()
                print 'File Edited Successfully!'
    else:
        print 'Error in getting file'
except urllib2.HTTPError, e:
    print e.fp.read()