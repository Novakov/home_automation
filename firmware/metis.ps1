werl -pa ebin -pa deps/*/ebin -boot start_sasl -sname me -setcookie pi -home_automation modes web -eval 'net_kernel:connect(pi@raspberrypi).'
