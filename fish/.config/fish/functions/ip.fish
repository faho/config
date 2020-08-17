# Defined in /tmp/fish.Lj32QA/ip.fish @ line 1
function ip --description 'alias ip ip -c'
    if isatty stdout
        command ip -c $argv
    else
        command ip $argv
    end
end
