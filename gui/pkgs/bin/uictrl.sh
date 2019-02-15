#!/usr/bin/env fish

function cycle
	set nsp_ws     (wmctrl -d | awk '{if ($9 == "NSP") {print $1}}')
	set current_ws (wmctrl -d | awk '{if ($2 == "*")   {print $1}}')

	wmctrl -l | awk -v current_ws=$current_ws -v nsp_ws=$nsp_ws '{if ($2 > current_ws && $2 != nsp_ws) {print $2}}' | uniq
	wmctrl -l | awk -v current_ws=$current_ws -v nsp_ws=$nsp_ws '{if ($2 < current_ws && $2 != nsp_ws) {print $2}}' | uniq
end

switch $argv[1]
	case next
		wmctrl -s (cycle)[1]
	case prev
		wmctrl -s (cycle | tac)[1]
end
