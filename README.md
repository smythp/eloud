## Eloud

A lightweight screen reader for Emacs.


### Overview

Eloud works on OSX and Linux versions of Emacs and uses the espeak speech synthesizer as the backend. It's designed to respond to user input, so if you move a word, character, or sentence, Eloud will read that segment as you move. 

### Installation

#### Manual install

##### Install espeak 

First, install espeak. On Ubuntu or Debian, use:

    sudo apt-get install espeak 
	
On OSX, use:

	brew install espeak 
	
Or find the compiled version [here](http://espeak.sourceforge.net/download.html)

##### Install the package

Clone this repo:

	cd ~
    git clone https://github.com/smythp/eloud.git
	
Add the load path to your .emacs:

    (add-to-list 'load-path "~/eloud/")
	
	
Finally, set the path to espeak by adding this to your .emacs:

	(setq eloud-espeak-path "~/eloud/")
	
#### Quick install

	cd ~
    git clone https://github.com/smythp/eloud.git
    (add-to-list 'load-path "~/eloud/")	
	(setq eloud-espeak-path "~/eloud/")	
	
#### Eloud or Emacspeak?

Use Eloud if:

- You want to turn speech on or off
- You have low vision or eye strain
- You have trouble installing Emacspeak

Use Emacspeak if:

- You don't need to turn speech on or off
- You want to handle more dge cases
- You want to use a speech server other than Espeak


