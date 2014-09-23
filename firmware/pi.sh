#!/bin/sh
erl -pa ebin -pz deps/*/ebin -s home_automation -boot start_sasl -sname pi -setcookie pi -home_automation modes [hw] -eval 'sync:go(), application:start(os_mon).'
