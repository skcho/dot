# prompt + terminal title
# NOTE: '\[', '\]' should be included for non-characters.

PS1="\[\e[1;32m\]\h\[\e[m\]:\[\e[1;34m\]\w\[\e[m\]\$ "
PS1="\[\e]0;\u@\h:\w\a\]$PS1"
