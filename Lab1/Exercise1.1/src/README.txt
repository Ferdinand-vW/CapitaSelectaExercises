This file contains the following for each implemented parallel strategy:
    - File name
    - Short description of the implemented strategy and how it differs from the sequential version
    - Number of cores used to run (I will try different number and only write down the best one)
    - Average run-time
    - Speed up compared to sequential version

All programs are compiled using the following:
ghc -O2 index.hs -rtsopts -threaded

They are run using the following command:
echo "concurrent parallel" | ./index docs/* +RTS -s -Nx -A128M

File name: Index.hs
Descr: Sequential version of the program
Nr cores: 4
Average run-time: 2.7394
Speed-up: 1

File name: Index1.hs
Descr: Calls to mkIndex are done with eval Monad
Nr Cores: 3
Average run-time: 1.7671
Speed-up: 1.55

File name: Index2.hs
Descr: Calls to mkIndex are done with par Monad
Nr Cores: 4
Average run-time: 1.7975
Speed-up: 1.52

File name: Index3.hs
Descr: Calls to mkIndex are done with eval Monad
and joinIndices is implemented using the eval Monad
Nr Cores: 3
Average run-time: 1.5188
Speed-up: 1.8

File name: Index4.hs
Descr: Calls to mkIndex are done with par Monad
and joinIndices is implemented using the eval Monad
Nr Cores: 3
Average run-time: 1.6144
Speed-up: 1.7

File name: Index5.hs
Descr: Calls to mkIndex are done with eval Monad
and joinIndices is implemented using the par Monad
Nr Cores: 4
Average run-time: 1.1895
Speed-up: 2.3

File name: Index6.hs
Descr: Calls to mkIndex are done with par Monad
and joinIndices is implemented using the par Monad
Nr Cores: 4
Average run-time: 1.2159
Speed-up: 2.25

The best speedup I was able to gain was 2.3 using a combination of eval and par
for mkIndex and joinIndices respectively.