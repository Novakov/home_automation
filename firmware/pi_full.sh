erl -pa ebin deps/*/ebin -boot start_sasl -sname pi -setcookie pi -s home_automation -home_automation modes [hw,web]