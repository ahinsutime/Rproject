from bs4 import BeautifulSoup
import requests
import csv

statenum=50

startaddr="https://www.ncdc.noaa.gov/cag/statewide/time-series/"
windowaddr=["/12/12","/all/1"]
endaddr="/1950-2018?base_prd=true&firstbaseyear=1901&lastbaseyear=2000"
midaddr=["/tavg","/tmax","/tmin"]
colnames=["tavg","tmax","tmin"]
windowtype=["annual","monthly"]
# tavg for avg temperature
# tmax for max temp
# tminf or min temp

for j in range(2):
    for i in range(3):
        filename=windowtype[j]+'_'+colnames[i]+'.csv'
        file=open(filename,'a',encoding="utf-8")
        wr=csv.writer(file,delimiter=',')
        headers=['state','year',colnames[i]]
        wr.writerow(headers)
        states=[]
        for n in range(statenum):
            sn=n+1
            if(sn==49):
                print('skip empty statenum')
            else:
                addr=startaddr+str(sn)+midaddr[i]+windowaddr[j]+endaddr
                page=requests.get(addr)
                soup=BeautifulSoup(page.content,'lxml')
                req=soup.find(id="required")
                reqrows=req.findAll("div",{"class":"row"})
                statereq=reqrows[5]
                curstate=statereq.find('select').find("option",{"selected":"selected"}).text
                states.append(curstate)
                print(windowtype[j],colnames[i],curstate)
                table=soup.find(id="valuesTable")
                body=table.find('tbody')
                rows=body.findAll('tr')
                for row in rows:
                    cols=row.findAll('td')
                    yearfield=cols[0].find('a').text
                    tempfield=cols[1].text
                    year=yearfield[:4]
                    temp=tempfield[:-2]
                    arow=[curstate,year,temp]
                    wr.writerow(arow)
                file.flush()
    
