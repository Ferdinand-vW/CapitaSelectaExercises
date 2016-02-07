read -p "Which version do you want to execute? Enter a number from 0 (index.hs) to 6 (index6.hs) " n
read -p "How many cores do you want to use? " x
case $n in
  0 ) echo "concurrent parallel" | ./dist/build/Index/Index src/docs/* +RTS -s -N$x -A128M;;
  * ) echo "concurrent parallel" | ./dist/build/Index$n/Index$n src/docs/* +RTS -s -N$x -A128M;;
esac