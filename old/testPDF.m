close all;
%plot(0:0.1:15,exppdf(0:0.1:15,1))
%figure;
##hist(rande(1,1000)+rande(1,1000)+rande(1,1000)+rande(1,1000)+rande(1,1000),100)
pop=zeros(1,1000);
for i=1:1000
  dice = rand;
  if(dice < 0.4)
    pop(i) = log(rand)/(-4);
  else
    pop(i) = log(rand)/(-1) + log(rand)/(-1);% + log(rand)/(-1);
  endif
endfor
hist(pop,200)