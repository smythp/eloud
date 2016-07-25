## Eloud

A lightweight, interactive screen reader for Emacs.


### Overview

Eloud works on OSX and Linux versions of Emacs and uses the espeak speech synthesizer as the backend. It's designed to respond to user input, so if you move a word, character, or sentence, Eloud will read that segment as you move. 

### Manual install

#### 1. Install espeak 

First, install espeak. On Ubuntu or Debian, use:

    sudo apt-get install espeak 
	
On OSX, use:

	brew install espeak 
	
Or find the compiled version [here](http://espeak.sourceforge.net/download.html)

### 2. Install the package

Clone this repo:

    cd ~
    git clone https://github.com/smythp/eloud.git
	
Add the load path to your .emacs:

    (add-to-list 'load-path "~/eloud/")
	
	
Finally, set the path to espeak by adding this to your .emacs:

For Debian/Ubuntu:

    (setq eloud-espeak-path "~//usr/bin/espeak/"

For OSX:

    (setq eloud-espeak-path "/usr/local/bin/espeak")  ; for OSX

Your espeak may be located elsewhere. In that case, use `which espeak` in the terminal to find where the executable is located.

	
#### Quick install

    d ~
    git clone https://github.com/smythp/eloud.git
    (add-to-list 'load-path "~/eloud/")	
    setq eloud-espeak-path "~/eloud/")	

### Using Eloud

Enable Eloud with `M-x eloud-mode`. Once enabled, use normal Emacs navigation keys to move around the buffer. As you move over characters, words, lines, and sentences, Eloud will read with you. To read the whole buffer, use the `beginning-of-buffer` function (bound to M-< by default). Eloud will also read minibuffer prompts, dabbrev completion, and spellcheck.

There are still many edge cases in Emacs not covered by Eloud. Please open issues or make pull requests when you encounter situations not covered by Eloud.


### Eloud or Emacspeak?

Use Eloud if:

- You want to turn speech on or off
- You have low vision or eye strain
- You have trouble installing Emacspeak

Use Emacspeak if:

- You don't need to turn speech on or off
- You want to handle more dge cases
- You want to use a speech server other than Espeak


