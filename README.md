## Eloud

A lightweight, interactive screen reader for Emacs. View the demo [here](https://www.youtube.com/watch?v=k5BLXMGSTJs).  

### Overview

Eloud works on Linux and OSX versions of Emacs and uses the espeak speech synthesizer as the backend. It's designed to respond to user input, so if you move a word, character, or sentence, Eloud will read that segment as you move. 

### Manual install

#### 1. Install espeak 

First, install espeak. On Ubuntu or Debian, you likely already have espeak. If that's not the case, run:

    sudo apt-get install espeak 
	
On OSX, use:

	brew install espeak 
	
You can try the compiled version [here](http://espeak.sourceforge.net/download.html), but it's difficult to install. I recommend [installing Homebrew ](https://coolestguidesontheplanet.com/installing-homebrew-on-os-x-el-capitan-10-11-package-manager-for-unix-apps/) and running the above command instead.

### 2. Install the package

Clone this repo:

    cd ~
    git clone https://github.com/smythp/eloud.git
	
Add the load path to your .emacs and require the code:

    (add-to-list 'load-path "~/eloud/")
    (require 'eloud)
	
Finally, set the path to espeak in your .emacs:

Debian/Ubuntu:

    (setq eloud-espeak-path "/usr/bin/espeak")

OSX:

    (setq eloud-espeak-path "/usr/local/bin/espeak") 

Your espeak may be located elsewhere. In that case, use `which espeak` in the terminal to find where the executable is located.
	
#### Quick install

    cd ~
    git clone https://github.com/smythp/eloud.git
	
Add to .emacs:

    (add-to-list 'load-path "~/eloud/")	
	(require 'eloud)
	(setq eloud-espeak-path "/usr/bin/espeak")

### Using Eloud

Enable Eloud with `M-x eloud-mode`. Once enabled, use normal Emacs navigation keys to move around the buffer. As you move over characters, words, lines, and sentences, Eloud will read with you. To read the whole buffer, use the `beginning-of-buffer` function (bound to M-< by default). Eloud will also read minibuffer prompts, dabbrev completion, and spellcheck.

There are still many edge cases and interactions with modes and packages that Eloud doesn't handle properly. Please open issues or make pull requests when you encounter these situations.


### Eloud or Emacspeak?

Use Eloud if:

- You want to turn speech on and off
- You have low vision or eye strain
- You have trouble installing Emacspeak but want to try Emacs with speech

Use Emacspeak if:

- You want speech on all the time
- You need to handle more edge cases
- You want to use a speech server other than Espeak


