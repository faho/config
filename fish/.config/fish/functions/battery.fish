# NAME
#   battery [arguments]
#
# SYNOPSIS
#   OS X and Linux compatible battery utility.
#
# USAGE
#   if available battery
#     battery
#   end
#
# ENV #   BATTERY_IS_PLUGGED
#   BATTERY_IS_CHARGING
#   BATTERY_TIME_LEFT     Time left in `HH:MM` format.
#   BATTERY_SLOTS         Number of slots/gauges in base 10.
#   BATTERY_MAX_CAP       Battery maximum capacity.
#   BATTERY_CUR_CAP       Battery current capacity.
#   BATTERY_PCT           Current battery life in %.
#
# ARGUMENTS
#   filled_slot_ch    ▮
#   empty_slot_ch     ▯
#   show_empty_slots  true

function init --on-event init_battery
  battery_update_info
end

function battery_update_info
	test -z "$OSTYPE"; and set OSTYPE (uname)
	if test $OSTYPE = "Darwin"
		battery_update_info_darwin
	else
		battery_update_info_linux
	end
end

function battery_update_info_linux
	if not type -q upower
		echo "Please install upower"
		return 1
	end
	set -l devices (upower -e | grep battery) # This can theoretically be multiple
	set -l upo (upower -i $devices)

	# Mind the space in " charging", we want to match "charging" but not "discharging"
	printf "%s"\n $upo | grep -q "state:.* charging\|state:.*fully-charged"
	and set -g BATTERY_IS_PLUGGED
	or set -e  BATTERY_IS_PLUGGED

	printf "%s"\n $upo \
	| grep -q "state:.* charging"
    and set -g BATTERY_IS_CHARGING
    or set -e  BATTERY_IS_CHARGING

	set -g BATTERY_MAX_CAP (printf "%s\n" $upo \
	| grep "energy-full:" \
	| cut -d ":" -f 2)

	set -g BATTERY_CUR_CAP (printf "%s\n" $upo \
	| grep "energy:" \
	| cut -d ":" -f 2)

	set -g BATTERY_PCT (printf "%s\n" $upo \
	| grep "percentage:" | cut -d":" -f2 | cut -d"%" -f1)

	set -g BATTERY_TIME_LEFT (printf "%s\n" $upo \
	| grep "time to" \
	| cut -d":" -f 2)

	set -g BATTERY_SLOTS (math $BATTERY_PCT / 10)
end

function battery_update_info_darwin
  set -l ioreg (ioreg -rc "AppleSmartBattery")

  printf "%s"\n $ioreg \
  | grep -q "\"ExternalConnected\" = Yes"
    and set -g BATTERY_IS_PLUGGED
    or set -e BATTERY_IS_PLUGGED

  printf "%s"\n $ioreg \
  | grep -q "\"IsCharging\" = Yes"
    and set -g BATTERY_IS_CHARGING
    or set -e  BATTERY_IS_CHARGING

  set -g BATTERY_MAX_CAP (printf "%s\n" $ioreg \
  | grep "\"MaxCapacity\" =" \
  | sed -e 's/^.*"MaxCapacity"\ =\ //')

  set -g BATTERY_CUR_CAP (printf "%s\n" $ioreg \
  | grep "\"CurrentCapacity\" =" \
  | sed -e 's/^.*CurrentCapacity"\ =\ //')
  set -g BATTERY_PCT (printf "%.1f" \
    (echo "$BATTERY_CUR_CAP / $BATTERY_MAX_CAP * 100" | bc -l))

  set -g BATTERY_TIME_LEFT (printf "%s\n" $ioreg \
  | grep "\"AvgTimeToEmpty\" =" \
  | sed -e 's/^.*"AvgTimeToEmpty"\ =\ //')
  set BATTERY_TIME_LEFT (printf "%02d:%02d" \
    (echo "$BATTERY_TIME_LEFT / 60" | bc) \
    (echo "$BATTERY_TIME_LEFT % 60" | bc))

  set -g BATTERY_SLOTS (math $BATTERY_PCT / 10)
end

function battery -a \
  filled_slot_ch    \
  empty_slot_ch     \
  show_empty_slots  \
  red yellow green

  if test -z "$filled_slot_ch"
    set filled_slot_ch ▮
  end

  if test -z "$empty_slot_ch"
    set empty_slot_ch ▯
  end

  if test -z "$show_empty_slots"
    set show_empty_slots true
  end

  if test -z "$red"
    set red (set_color -o f00)
  end

  if test -z "$yellow"
    set yellow (set_color -o ff0)
  end

  if test -z "$green"
    set green (set_color -o 0f0)
  end

  set -l normal   (set_color normal)
  set -l color    $green

  battery_update_info
  switch $BATTERY_SLOTS
    case 0 1 2; set color $red
    case 3 4;   set color $yellow
    case "*";   set color $green
  end

  for n in (seq 10)
    if test $n -le $BATTERY_SLOTS
      printf "$color$filled_slot_ch$normal"
    else
      if test $show_empty_slots = true
        printf "$color$empty_slot_ch$normal"
      end
    end
  end
  
end
