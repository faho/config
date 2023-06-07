function gws --wraps='git status --short' --description 'alias gws=git status --short'
  git status --short $argv; 
end
