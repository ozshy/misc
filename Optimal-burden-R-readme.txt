 Note, the code does not conduct any of the rounding. It simply calculates the optimal burden for the data we are using and for amounts between [$4, $5]. If you want to run the restricted calculations, then you simply need to switch "only_20note" to TRUE in the functions.

I should warn you that trying to solve the model for every transaction between $0.01 and $100 would take hours to run even with the parallel computing. This is even more important for you to know because I don't think R can run efficiently in parallel with Windows. You will need a machine that is MacOS or Linux based.
