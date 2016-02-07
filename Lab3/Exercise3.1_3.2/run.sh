read -p "Which version do you want to run? Bounded Chan(b), TChan(t) or Concurrent Chan(c)? " v
case $v in
  b ) ./dist/build/BChan/BChan +RTS -s;;
  t ) ./dist/build/TChan/TChan +RTS -s;;
  c ) ./dist/build/ConcChan/ConcChan +RTS -s;;
esac