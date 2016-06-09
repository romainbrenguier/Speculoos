#include "aiger2c.h"

int main (){

  int input[1] = {1};
  int latch[1] = {0};
  int output[1] = {0};
  int i;

  for(i = 0; i < 100; i++)
    {
      printf("o:%d\n",output[0]);
      printf("l:%d\n",latch[0]);
      input[0] = (i % 5 > 2)?0:1;
      printf("i:%d\n",input[0]);
      update(input,latch,output);
    }

  return(0);
}
