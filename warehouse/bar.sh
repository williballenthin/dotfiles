#!/bin/sh

INTERVAL=1;

DATE_FORMAT='%A, %Y-%m-%d %H:%M:%S'

fdate() {
    date "+$DATE_FORMAT";
}

getmemtotal() {
    free -h | grep Mem | awk '{ print $2}';
}

getmemused() {
    free -h | grep Mem | awk '{ print $3}';
}

getcpuload() {
    cat /proc/loadavg | cut -d " " -f 1-3;
}

while true; do 
    ### DATE
    DATE=$(fdate);

    ### CPU
    LOAD=$(getcpuload);

    echo "cpu: ${LOAD}  |  Mem: $(getmemused) / $(getmemtotal)  |  ^fg(white)${DATE}^fg()";
    sleep $INTERVAL;
done;
