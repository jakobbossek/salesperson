#!/bin/bash

read -p "Do you wish to install salesperson? (y/n)" yn
case $yn in
  y)
    echo "Installing salesperson ...";
    Rscript -e "install.packages('devtools', dep = TRUE)";
    Rscript -e "devtools::install_github('jakobbossek/salesperson')";;
  n) ;;
esac

read -p "Do you wish to download solvers to ~/.config/salesperson? (y/n)" yn
case $yn in
  y)
    echo "Downloading solvers ...";
    mkdir -vp ~/.config/salesperson;
    # donwload the "latest" folder version only, no cloning!
    svn export --force https://github.com/wwu-wi/TSPAS/trunk/solvers ~/.config/salesperson/solvers;
    # now install solvers
    for dir in ~/.config/salesperson/solvers/*
    do
      cd $dir
      # check for directory
      test -d "$dir" || continue
      # check for build files and execute
      [[ -f Makefile ]] && make
      [[ -f build.exe ]] && ./build.exe
    done;;
  n)
    exit;;
esac
