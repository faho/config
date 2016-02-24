function fish_user_key_bindings
    bind \cx "commandline | xsel --clipboard"
    bind \cv "commandline -i (xsel --clipboard) ^/dev/null"
    bind -k ic yank # insert key
    bind \ev yank-pop
	bind \cr 'fish_replace_regex'
end
