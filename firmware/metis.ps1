$deps = ls deps | % { "./deps/$_/ebin" }    

werl -pa ebin $deps -boot start_sasl -sname me -setcookie pi -s home_automation -home_automation modes [web] -home_automation dbserver "`"localhost`"" -eval 'net_kernel:connect(pi@raspberrypi), application:start(os_mon), sync:go().' -new_console
