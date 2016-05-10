/* parallel.c
   $ gcc -o parallel parallel.c -fopenmp -lm -O2 -Wall -W -ansi -pedantic
   $ ./parallel
   PROC: 4
   primes: 78498
*/
#include <stdio.h>
#include <math.h>
#include <omp.h>

/* 소수 판정 */
int is_prime (int n) {
  const int m = floor(sqrt(n));
  int i = 0;
  for (i = 2; i <= m; i++) if (0 == n % i) return 0;
  return 1;
}

int main () {
  int arr[1000001];
  { /* 1000000까지의 수(실질적으로 사용하는 것은 2~)*/
    unsigned int i = 0;
    for (i = 0; i < sizeof(arr)/sizeof(int); i++)
      arr[i] = i;
  }

#ifdef _OPENMP
  /* OpenMP으로 병렬 실행하는 때는 프로세서 수를 표시 */
  printf("PROC: %d\n", omp_get_num_procs());
#endif

  { /* arr의 각 요소에 is_prime를 적용 */
    unsigned int i = 0;
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (i = 2; i < sizeof(arr)/sizeof(int); i++)
      arr[i] = is_prime(arr[i]);
  }

  { /* 소수를 센다 */
    int primes = 0;
    unsigned int i = 0;
    for (i = 2; i < sizeof(arr)/sizeof(int); i++)
      primes += arr[i];
    printf("primes: %d\n", primes);
  }

  return 0;
}

    
