#!/bin/bash

set -f

loop=0
last_time=0
last_use=(0 0 0 0)
last_idle=(0 0 0 0)
last_rx=0
last_tx=0
cpu_util=(0 0 0 0)

while true; do
    if [ $((loop % 600)) -eq 0 ]; then
	weather=$(curl "wttr.in?format=2")
	ticker=$(bash /home/dnw/ticker.sh DIA VOO QQQ BTC-USD)
	ticker=${ticker:2}
    fi

    cpu_util=($(mpstat -P ALL 1 1 | awk '/Average:/ && $2 ~ /[0-9]/ {print $3}')) # waits for one second for data to come in
    mfree=$(free -m | sed '2q;d' | awk '{print $3}')
    total=$(free -m | sed '2q;d' | awk '{print $2}')
    total=$(bc -l <<< "${total}/1000" | cut -c -4)

    if [ $mfree -gt 999 ]; then
	mfree=$(bc -l <<< "${mfree}/1000" | cut -c -4)
	mfree="${mfree}GiB/7.89GiB"
    else
	mfree="${mfree}MiB/7.89GiB"
    fi

    dfree=$(btrfs filesystem usage /  2> /dev/null | sed '3q;d' | awk '{print $3}')

    tx=$(ifconfig eno1 | sed '7q;d' | awk '{print $4}' | sed 's/bytes://')
    tx_delta=$(($tx-$last_tx))
    last_tx=$tx
    if [ $tx_delta -lt 1000 ]; then
	tx_delta="${tx_delta}B/s"
    else
	if [ $tx_delta -lt 1000000 ]; then
	    tx_delta=$(bc -l <<< "${tx_delta}/10^3")
	    tx_delta="${tx_delta:0:3}kB/s"
	else
	    tx_delta=$(bc -l <<< "${tx_delta}/10^6")
	    tx_delta="${tx_delta:0:3}MB/s"
	fi
    fi

    rx=$(ifconfig eno1 | sed '7q;d' | awk '{print $2}' | sed 's/bytes://')

    rx_delta=$(($rx-$last_rx))

    last_rx=$rx
    if [ $rx_delta -lt 1000 ]; then
	rx_delta=$(echo ${rx_delta} | sed -e :a -e 's/^.\{1,3\}$/ &/;ta')
	rx_delta="${rx_delta:0:4}B/s"
    else
	if [ $rx_delta -lt 1000000 ]; then
	    rx_delta=$(bc -l <<< "${rx_delta}/10^3")
	    rx_delta=$(echo ${rx_delta} | sed -e :a -e 's/^.\{1,3\}$/ &/;ta')
	    rx_delta="${rx_delta:0:4}kB/s"
	else
	    rx_delta=$(bc -l <<< "${rx_delta}/10^6")
	    rx_delta=$(echo ${rx_delta} | sed -e :a -e 's/^.\{1,3\}$/ &/;ta')
	    rx_delta="${rx_delta:0:4}MB/s"
	fi
    fi

    datetime=$(date "+%a %-d %b %-I:%M %p")

    xsetroot -name "  <span foreground=\"#114477\"></span> ( ${cpu_util[0]}%  ${cpu_util[1]}%  ${cpu_util[2]}%  ${cpu_util[3]}% ) | <span foreground=\"#114477\"></span> ${mfree} | <span foreground=\"#114477\"></span> ${dfree}/1.80TiB | ${tx_delta}<span foreground=\"#114477\"></span> ${rx_delta}<span foreground=\"#114477\"></span>   ${datetime} ; ${weather} ;${ticker} "

    ((loop++))
done
