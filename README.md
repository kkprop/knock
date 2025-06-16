- Knock
=======

- Usage:
    - bb -m knock.main
    - bb -m knock.main -h
    - change directory to the folder
        - write to Roam:
            - bb write-roam Hi from shell
        - query wordnet
            - bb wordnet reality
            - bb wordnet Lao Zi
        - convert asciinema to PowerPoint:
            - bb asciinema2ppt --input recording.cast --output presentation.pptx
    - put alias into .bashrc or .zshrc or ...
        - · 
            - wn='bb --config ~/kkprop/knock/bb.edn wordnet'
            - roam="bb --config ~/kkprop/knock/bb.edn bb write-roam"
        - then in the shell simply query wordnet 
            - wn Hesse
            - wn Hermann von Helmholtz 
    - one command display off
      - bb display-off
      - also setting alias shortcut: bb alias doff "cd ~/kkprop/knock && bb display-off" 
- dependency
    - [babashka](https://github.com/babashka/babashka/releases)
            - brew install borkdude/brew/babashka
    - [selenium](https://www.selenium.dev/)
        - For OSX
            - brew install chromedriver
    - [roam research](https://relemma-git-feat-frontdesk.roamresearch.com/)
        - using links 👆 to get access token of your Graph
          - which located in Roam Research: · · · in the up right corner -> Settings > Graph Tab -> API Tokens
    - epub-tools
        - brew install ebook-tools
        - sudo apt-get install epub-utils
    - pdf tools
        - brew install poppler 
        - sudo apt-get install pdftohtml
    - pandoc
        - brew install pandoc
        - sudo apt-get install pandoc
    - For asciinema2ppt:
        - pip install python-pptx
        - brew install imagemagick
        - npm install -g termtosvg

- Lucy
======

# An etaoin browser for web pages navigation#
  * the browser is at background
  * things requested are piped sequencially
  * request can be repeated.
  

