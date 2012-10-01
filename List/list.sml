fun length lst = case lst of nil => 0
                       | (h::t) => 1 + length t;

length [] = 0;
length [1] = 1;
length [1,2] = 2;


