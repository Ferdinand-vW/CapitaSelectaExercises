read -p "Which version do you want to run? Pick a number from 0 (bingtranslator.hs) to 2 (bingtranslator2.hs) " n
case $n in
  0 ) ./dist/build/BingTranslator/BingTranslator "translate this";;
  * ) ./dist/build/BingTranslator$n/BingTranslator$n "translate this";;
esac