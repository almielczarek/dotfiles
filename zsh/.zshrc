# If you come from bash you might have to change your $PATH.
export PATH="$HOME/.local/bin:$HOME/.scripts:$PATH:$HOME/.cargo/bin:$HOME/.npm-global/bin"

# Path to your oh-my-zsh installation.
export ZSH="/home/alex/.oh-my-zsh"

export EDITOR=nvim

export ZSH_THEME="lambda"

plugins=(git)

source $ZSH/oh-my-zsh.sh

bl () {
  x=$(echo "1060*$1/100" | bc)
  echo "$x" | sudo tee /sys/class/backlight/intel_backlight/brightness
}

tykma () {
  cp $@ /mnt/rwdata/Machining/TYKMA/Incoming\ Files
}

zall () {
  pdfunite *.pdf(n) /dev/stdout | zathura -
}

pdf2svgopt () {
    parallel -j 8 inkscape -o {.}.svg {} ::: $@
    parallel -j 8 svgo -i {.}.svg -o {.}-opt.svg ::: $@
    # parallel -j 8 sed -i "s/stroke-width=\".*\"/stroke-width=\"1\"/g" {.}-opt.svg ::: $@
    parallel -j 8 mv {.}-opt.svg {.}.svg ::: $@
}

alias frequency="cpupower frequency-info"
alias powersave="sudo cpupower frequency-set -g powersave"
alias performance="sudo cpupower frequency-set -g performance"

alias rw=". ~/.scripts/rwdata.sh"
alias urw=". ~/.scripts/unmount-rwdata.sh"
alias vim="nvim"
alias vi="nvim"
alias em="emacsclient -nw -a '' -c"
alias ls="exa -lh"

LS_COLORS='rs=0:di=01;36:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:';
export LS_COLORS

vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

find-file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
