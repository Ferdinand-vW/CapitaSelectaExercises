To build: ./build.sh
To run: ./run.sh

This folder contains the exercises 2.1 and 2.2 of Lab 2

The implementation of exercise 2.1 can be found in bingtranslator1.hs
Notes:
Interestingly simply making use of the async api sped up the program by a factor of 10.
From about 20 seconds to about 2 seconds. Also using more than 1 core slowed down the program,
which is most likely caused by having too much overhead.

The implementation of exercise 2.2 can be found in bingtranslator2.hs
Exercise 2.2 further improves the performance of the program to about 1.4s


P.S. It seems that on my linux computer BingTranslate.hs is occasionally giving an error.

