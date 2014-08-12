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
    free -h | grep "cache:" | awk '{ print $3}';
}

getcpuload() {
    cat /proc/loadavg | cut -d " " -f 1-3;
}

getchatcount() {
    wc -l ~/.weechat/highlights.txt | cut -d " " -f 1;
}

getbattery() {
    bash ~/.path/battery;
}

while true; do 
	echo "battery: $(getbattery) | chat: $(getchatcount) |  cpu: $(getcpuload)  |  Mem: $(getmemused) / $(getmemtotal)  |  ^fg(white)$(fdate)^fg()  ";
    sleep $INTERVAL;
done;
