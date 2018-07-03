#!/bin/bash

nmcli conn down "eduroam"
nmcli conn down "Imperial-WPA"
sudo /sbin/ifup enp2s0
