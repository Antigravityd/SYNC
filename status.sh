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


    now_time=$(date +%s)
    for i in {0..3}
    do
	ln=$((i+2))
	usage=$(sed "${i+2}q;d" /proc/stat)
	IFS=" " read -ra usage_arr <<< "$usage"

	now_use=$((usage_arr[1] + usage_arr[2] + usage_arr[3] + usage_arr[6] + usage_arr[7] + usage_arr[8]))
	now_idle=$((usage_arr[4] + usage_arr[5]))

	delta_time=$((now_time-last_time))
	delta_use=$((now_use-last_use[i]))
	delta_idle=$((now_idle-last_idle[i]))
	delta_total=$((delta_use+delta_idle))

	last_use[i]=${now_use}
	last_idle[i]=${now_idle}

	cpu_util[i]=$(bc -l <<< "100*${delta_use}/${delta_total}/${delta_time}" | cut -c -4)
    done
    last_time=$(date +%s)
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

    tx=$(ifconfig eno1 | sed '7q;d' | awk '{print $2}' | sed 's/bytes://')
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
	rx_delta="${rx_delta}B/s"
    else
	if [ $rx_delta -lt 1000000 ]; then
	    rx_delta=$(bc -l <<< "${rx_delta}/10^3")
	    rx_delta="${rx_delta:0:3}kB/s"
	else
	    rx_delta=$(bc -l <<< "${rx_delta}/10^6")
	    rx_delta="${rx_delta:0:3}MB/s"
	fi
    fi

    datetime=$(date "+%a %-d %b %-I:%M %p")

    xsetroot -name "  <span foreground=\"#114477\"></span> ( ${cpu_util[0]}%  ${cpu_util[1]}%  ${cpu_util[2]}%  ${cpu_util[3]}% ) | <span foreground=\"#114477\"></span> ${mfree} | <span foreground=\"#114477\"></span> ${dfree}/1.80TiB | ${tx_delta}<span foreground=\"#114477\"></span> ${rx_delta}<span foreground=\"#114477\"></span>   ${datetime} ; ${weather} ;${ticker} "

    ((loop++))
    sleep 1
done
