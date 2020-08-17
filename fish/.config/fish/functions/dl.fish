# Defined in /tmp/fish.Vb6txe/dl.fish @ line 2
function dl
    curl -L --remote-name-all -C - --limit-rate 300K $argv
end
