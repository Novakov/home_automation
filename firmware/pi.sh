#!/bin/sh
erl -pa ebin -pz deps/*/ebin -s home_automation -boot start_sasl -sname pi -setcookie pi -home_automation modes [hw] -home_automation dbserver "\"192.168.1.3\"" -eval 'sync:go(), application:start(os_mon).'
