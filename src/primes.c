#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main()
{
  int n;
  scanf("%d", &n);

  int* prime = (int*)malloc(sizeof(int)*(n+1));
  //Loading the array with numbers from 1 to n
  for(int i = 1; i <= n; i++)
    {
      prime[i] = i;
    }
  //Start with least prime number, which is 2.
  //No need to check for numbers greater than square root of n.
  //They will be already marked.
  for(int i = 2; i <= sqrt(n); i++)
    {
      if(prime[i] != -1)
	{
	  //Mark all the multiples of i as -1.
	  for(int j = 2*i; j <=n ; j += i)
	    prime[j] = -1;
	}
    }
  int count = 0;
  for(int i=2; i <= n; i++)
	{
	  if(prime[i] != -1)
	    {
	      count++;
	    }
	}
  printf("%d\n",count);
}
