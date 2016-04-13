###### Examine the data
### sonoma-data-net.csv
# number of unique node ids: 31 (abnormal: nodeid 135 is not in mote-location-data)
tail -n +2 ../data/sonoma-data-net.csv | cut -d',' -f3 | sort | uniq -c

# time range: 2004/5/7 - 2004/6/2
tail -n +2 ../data/sonoma-data-net.csv | cut -d',' -f1 | cut -d ' ' -f1 | sort | uniq -c
# epoch range: 2812 - 10288 (7477 records)
tail -n +2 ../data/sonoma-data-net.csv | cut -d',' -f2 | cut -d ' ' -f1 | sort | uniq -c | sort -n -k2


### sonoma-data-all.csv
# number of unique node ids: 73 (abnormal: 65535; nodeid 138 in -net but not in -log)
tail -n +2 ../data/sonoma-data-all.csv | cut -d',' -f3 | sort | uniq -c

# epoch range outside -net.csv: 5157 (Note: total with network is all records in -all/-log.csv)
tail -n +2 ../data/sonoma-data-all.csv | cut -d',' -f2 | cut -d ' ' -f1 | sort | uniq -c | awk '$2 > 10288 || $2 < 2812' | wc -l



### sonoma-data-log.csv
# number of unique node ids: 72 (65535)
tail -n +2 ../data/sonoma-data-log.csv | cut -d',' -f3 | sort | uniq -c

# epoch range outside -net.csv: 5157 (Note: total with network is all records in -all/-log.csv)
tail -n +2 ../data/sonoma-data-log.csv | cut -d',' -f2 | cut -d ' ' -f1 | sort | uniq -c | awk '$2 > 10288 || $2 < 2812' | wc -l



