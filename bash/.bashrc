### ~/.bashrc for users
### contains only customization to differentiate from root

# If not running interactively, don't do anything (else slim breaks by calling tmux)
[[ $- != *i* ]] && return

# Source system-wide config 
# Do this first so user-config takes precedence
source /etc/bash.bashrc

# source ~/.shrc
#########################
# Variables and aliases #
#########################

# Nice color prompt
# \342\226\270 is "▸"
export PS1='\h\[\e[1;34m\]\w\[\e[m\] \[\e[1;32m\]\$\[\e[m\] ' # <Time as HH:MM:SS> <$HOSTNAME> <$PWD in blue> <Prompt Sign in Yellow> <Rest in default light white>

# prompt () {
#     if [ $? -eq "0" ]; then
#         exitcol="1;97";
#     else
#         exitcol="1;91";
#     fi
#     movecursor="\[\e["$(($COLUMNS-7))"G\]"
#     PS1="\[\e[1;97m\]\u\[\e["$exitcol"m\]@\[\e[1;97m\]\h: \w"$movecursor"\t\n\$\[\e[0m\] "
# }
# PROMPT_COMMAND=prompt

source /usr/share/bash-completion/completions/systemctl
_usystemctl() {
	_systemctl "--user" "$@"
}
complete -F _usystemctl usys
complete -F _systemctl sys
complete -F __get_active_units stop
complete -F __get_startable_units start
complete -F __get_active_units restart

# red="\[\e[31m\]"
# yellow="\[\e[33m\]"
# magenta="\[\e[35m\]"
# cyan="\[\e[36m\]"
# reset="\[\e[m\]"
# export PS1="${red}\u${yellow}@${cyan}\h${yellow}\w${magenta}\$${reset}"
