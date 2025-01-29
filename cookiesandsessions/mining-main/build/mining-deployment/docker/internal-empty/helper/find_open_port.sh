#! /bin/bash

#increments a given base port until it is usable. Echos the usable port.

BASE_PORT=$1
INCREMENT=1

port=$BASE_PORT
isfree=$(netstat -tln | grep -w $port | grep -i LISTEN)

while [[ -n "$isfree" ]]; do
    port=$((port+INCREMENT))
    isfree=$(netstat -tln | grep $port)
done

echo "$port"
