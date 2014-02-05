


.PHONY: pycheckers
pycheckers: 
  sudo pip install pep8 pyflakes
  sudo cp ./warehouse/pycheckers.py-113 /opt/pycheckers.py
  sudo ln -s /opt/pycheckers.py /usr/local/bin/pycheckers


.PHONY: remove_pycheckers
remove_pycheckers:
  sudo unlink /usr/local/bin/pycheckers
  sudo rm /opt/pycheckers.py
  sudo pip remove pep8 pyflakes


.PHONY: all
all: pycheckers


.PHONY: remove_all
remove_all: remove_pycheckers




