The solution of exercise 3.1 can be found in boundedchan.hs

I've also done some performance testing.
I compare Chan, TChan and (my implemented) BoundedChan
For each test I let a thread write 10million integers
into a channel and let another thread continuously read from that channel.
The program stops when the writing has been completed. For the BoundedChan
I will try out several values for the size of the channel.


BoundedChan:

#size = 10000000
average run-time = 4.2792

I was forced to give the BoundedChan the size of the number of integers i'm writing,
because a lower size would cause a deadlock.

TChan:
average run-time = 3.4429

It makes sense for BoundedChan to be slower than TChan. The size of BoundedChan didn't matter,
but because there now was a extra TVar that was being written to by both the reading thread and
the writing thread. This meant that if both threads tried to write at the same time one of them would
be invalidated and thus would take more time to complete the action it was performing.

Chan:
average run-time = 2.6568

Chan was the fastest of the three. Chan is implemented using MVar's and not TVar (TChan, BoundedChan).
Chan works very well in this example, because it has two MVar's for the writing and reading end of the channel and each of the two thread's will only access one. The writing thread will only access the writing MVar and the reading thread will only access the reading MVar. This means that neither thread
will be at any point be blocked, unless reading goes faster than writing. In otherwords, reading and
writing happens completely concurrent.
You could say the same for TChan, it has a reading end for the reading thread and a writing end for the
writing thread. However, after each transaction a log has to be verified and committed. This costs time, which is why I think that TChan is slower than Chan. 

In the case that we might have more than one thread reading from the channels, then I could see TChan performing better than Chan. Chan's reading MVar will be blocking, whereas TChan's reading end won't cause invalidation.


